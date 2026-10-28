;;; pilish-agents.el --- Subagent and workflow control menu -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Daniel Nouri

;; Author: Daniel Nouri <daniel.nouri@gmail.com>
;; Maintainer: Daniel Nouri <daniel.nouri@gmail.com>
;; URL: https://github.com/dnouri/pilish
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; `pilish-agents-menu' (C-c C-a, or A in `pilish-menu') is a transient
;; for the pi-subagents extension: it lists the session's subagents and
;; workflows, shows the selected one's status and recent output, and
;; steers, interrupts, stops, resumes, or spawns agents and pauses, kills,
;; skips, or retries workflow steps.
;;
;; State comes from the first available source:
;;
;;   feed  The `subagents:state' extension widget (see below).  Full
;;         fidelity; controls are sent as `/agents VERB ...' commands.
;;   disk  Child session files whose header `parentSession' names this
;;         session.  Real but read-only: status is inferred from the file.
;;   demo  An animated in-memory fleet (D toggles it).  Controls mutate
;;         the demo and echo the command they would send.
;;
;; Feed contract (what pi-subagents must publish in RPC mode)
;; ---------------------------------------------------------
;;
;; ctx.ui.setWidget("subagents:state", [JSON.stringify(state)]) whenever
;; state changes (pi-subagents checks every 500ms, in RPC mode only).
;; Pi forwards string-array widgets over RPC as `extension_ui_request'
;; method `setWidget'; they are not persisted to the session file.
;; State is one JSON object:
;;
;;   {version: 1, generatedAt: ms,
;;    controls: [verb...],             ; verbs below that are supported
;;    types: [{name, description?}],
;;    agents: [Agent], workflows: [Workflow]}
;;
;;   Agent: {id, name?, handle?, type, description, status, background,
;;           model?, thinking?, toolUses, turnCount?, maxTurns?,
;;           tokens, cost?, startedAt, completedAt?, activity?,
;;           recent: [{kind, text}], resultPreview?, error?,
;;           sessionFile?, sessionId?, outputFile?}
;;     status: queued|running|completed|steered|aborted|stopped|
;;             interrupted|error
;;     recent.kind: text|tool|steer (also thinking|status here), newest last
;;
;;   Workflow: {id, name, description?, status, startedAt, endedAt?,
;;              pausedMs, pausedAt?, phases: [{index, title}], phaseIndex,
;;              agentCount, doneCount, totalTokens?, totalToolCalls?,
;;              scriptPath?, journalPath?, logs: [string],
;;              agents: [{index, label, phaseIndex?, state, agentType?,
;;                        model?, toolCalls?, tokens?, durationMs?,
;;                        resultPreview?, error?, sessionFile?,
;;                        sessionId?}]}
;;     status: running|paused|completed|failed|killed
;;     agents.state: queued|running|done|failed|skipped|blocked|
;;                   interrupted
;;
;; Times are milliseconds since the epoch.  Control commands, sent as
;; prompts that pi runs immediately even while streaming:
;;
;;   /agents steer ID MESSAGE         /agents interrupt ID
;;   /agents stop ID                  /agents resume ID PROMPT
;;   /agents spawn TYPE PROMPT        /agents state   (republish)
;;   /agents workflow pause|resume|kill RUN-ID
;;   /agents workflow skip|retry RUN-ID INDEX

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'transient)
(require 'pilish-core)
(require 'pilish-ui)
(require 'pilish-menu)

(declare-function pilish-open-subagent-snapshot "pilish-snapshot" (&optional target))
(declare-function parse-iso8601-time-string "parse-time" (date-string &optional form))
(declare-function pilish--handle-extension-ui-request "pilish-render" (event))

;;;; Customization

(defgroup pilish-agents nil
  "Subagent and workflow control for Pilish."
  :group 'pilish
  :prefix "pilish-agents-")

(defcustom pilish-agents-feed-widget-key "subagents:state"
  "Extension widget key that carries pi-subagents state as JSON."
  :type 'string)

(defcustom pilish-agents-control-command "agents"
  "Pi slash command, without slash, that accepts agent control verbs."
  :type 'string)

(defcustom pilish-agents-refresh-interval 1.0
  "Seconds between live refreshes while the agents menu is open."
  :type 'number)

(defcustom pilish-agents-recent-finished 5
  "Finished agents shown by the default `recent' filter."
  :type 'integer)

(defcustom pilish-agents-recent-lines 6
  "Recent output lines shown for the selected agent."
  :type 'integer)

(defcustom pilish-agents-disk-running-seconds 45
  "A child session written within this many seconds may be running."
  :type 'integer)

(defface pilish-agents-running '((t :inherit success))
  "Face for running agents." :group 'pilish-agents)
(defface pilish-agents-queued '((t :inherit shadow))
  "Face for queued agents." :group 'pilish-agents)
(defface pilish-agents-done '((t :inherit font-lock-doc-face))
  "Face for completed agents." :group 'pilish-agents)
(defface pilish-agents-failed '((t :inherit error))
  "Face for failed agents." :group 'pilish-agents)
(defface pilish-agents-halted '((t :inherit warning))
  "Face for interrupted, stopped, or paused work." :group 'pilish-agents)
(defface pilish-agents-type '((t :inherit font-lock-type-face))
  "Face for agent types." :group 'pilish-agents)
(defface pilish-agents-title '((t :inherit bold))
  "Face for agent descriptions." :group 'pilish-agents)
(defface pilish-agents-meta '((t :inherit shadow))
  "Face for secondary agent details." :group 'pilish-agents)
(defface pilish-agents-selected '((t :inherit highlight :weight bold :extend nil))
  "Face for the selected row." :group 'pilish-agents)
(defface pilish-agents-activity '((t :inherit font-lock-keyword-face))
  "Face for current agent activity." :group 'pilish-agents)
(defface pilish-agents-tool '((t :inherit font-lock-function-name-face))
  "Face for tool lines in recent output." :group 'pilish-agents)
(defface pilish-agents-steer '((t :inherit font-lock-string-face))
  "Face for steering lines in recent output." :group 'pilish-agents)
(defface pilish-agents-rule '((t :inherit shadow))
  "Face for detail panel rules." :group 'pilish-agents)
(defface pilish-agents-badge '((t :inherit warning :weight bold :inverse-video t))
  "Face for the stub-data badge." :group 'pilish-agents)
(defface pilish-agents-heading '((t :inherit transient-heading))
  "Face for menu headings." :group 'pilish-agents)

;;;; State access

(defvar pilish-agents--chat-buffer nil
  "Chat buffer whose agents the open menu shows.")

(defvar-local pilish-agents--selected nil
  "Key of the selected item: (agent . ID) or (workflow . ID).")

(defvar-local pilish-agents--filter 'recent
  "Agent list filter: `recent', `active', or `all'.")

(defvar-local pilish-agents--demo nil
  "Demo fleet plist, or nil when demo mode is off.")

(defvar-local pilish-agents--view nil
  "Last computed view plist (:source :agents :workflows :controls :types).")

(defvar pilish-agents--disk-cache (make-hash-table :test #'equal)
  "Cache of parsed child sessions keyed by file.
Values are (STAMP . AGENT) where STAMP is the file's mtime and size.")

(defvar pilish-agents--header-cache (make-hash-table :test #'equal)
  "Cache of first-line session headers keyed by file.")

(defvar pilish-agents--timer nil
  "Refresh timer while the agents menu is open.")

(defvar pilish-agents--tick 0
  "Refresh counter, used for spinner frames.")

(defvar pilish-agents--rendered-keys nil
  "Item keys the open transient's suffixes were built for, by kind.")

(defun pilish-agents--chat ()
  "Return the live chat buffer for the agents menu or signal."
  (let ((buf (or (and (buffer-live-p pilish-agents--chat-buffer)
                      pilish-agents--chat-buffer)
                 (pilish--get-chat-buffer))))
    (unless (buffer-live-p buf)
      (user-error "No pi session in current buffer"))
    buf))

(defmacro pilish-agents--in-chat (&rest body)
  "Evaluate BODY in the agents menu chat buffer."
  (declare (indent 0) (debug t))
  `(with-current-buffer (pilish-agents--chat) ,@body))

(defun pilish-agents--now-ms ()
  "Return the current time in epoch milliseconds."
  (truncate (* 1000 (float-time))))

(defun pilish-agents--iso-ms (string)
  "Return ISO 8601 STRING as epoch milliseconds, or nil."
  (when (stringp string)
    (require 'parse-time)
    (condition-case nil
        (truncate (* 1000 (float-time (parse-iso8601-time-string string))))
      (error nil))))

(defun pilish-agents--get (item key)
  "Return KEY of ITEM, mapping JSON null and false to nil."
  (let ((value (plist-get item key)))
    (unless (memq value '(:null :false :json-false)) value)))

;;;; Sources: feed

(defun pilish-agents--parse-feed (lines)
  "Parse feed widget LINES into a state plist, or nil when malformed."
  (when lines
    (condition-case nil
        (let ((state (json-parse-string (string-join lines "\n")
                                        :object-type 'plist
                                        :array-type 'list
                                        :null-object nil
                                        :false-object nil)))
          (and (plistp state) (plist-get state :version) state))
      (json-error nil))))

(defun pilish-agents--feed-state ()
  "Return the parsed feed state in the current chat buffer, or nil."
  (pilish-agents--parse-feed
   (pilish--extension-widget-lines pilish-agents-feed-widget-key)))

;;;; Sources: disk

(defun pilish-agents--file-header (file)
  "Return FILE's parsed first-line session header, cached."
  (or (gethash file pilish-agents--header-cache)
      (puthash file
               (with-temp-buffer
                 (condition-case nil
                     (progn
                       (insert-file-contents file nil 0 8192)
                       (goto-char (point-min))
                       (or (json-parse-string
                            (buffer-substring (point) (line-end-position))
                            :object-type 'plist :null-object nil
                            :false-object nil)
                           'none))
                   (error 'none)))
               pilish-agents--header-cache)))

(defun pilish-agents--content-blocks (message)
  "Return MESSAGE content as a list of block plists."
  (let ((content (plist-get message :content)))
    (cond ((stringp content) (list (list :type "text" :text content)))
          ((listp content) content))))

(defun pilish-agents--tool-summary (name args)
  "Return a one-line summary of tool NAME called with ARGS."
  (let ((detail (or (plist-get args :command) (plist-get args :path)
                    (plist-get args :pattern) (plist-get args :description)
                    (plist-get args :query) (plist-get args :url) "")))
    (string-trim
     (format "%s %s" name
             (car (split-string (format "%s" detail) "\n"))))))

(defun pilish-agents--text-lines (text)
  "Return the non-blank lines of TEXT."
  (seq-remove #'string-blank-p (split-string (or text "") "\n")))

(defun pilish-agents--parse-child-session (file)
  "Parse child session FILE into an agent plist."
  (let ((recent nil) (tools 0) (turns 0) (cost 0.0) handle title prompt
        model ctx last-text stop started last-at sid)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (while (not (eobp))
        (let* ((line (buffer-substring-no-properties (point) (line-end-position)))
               (entry (and (not (string-empty-p line))
                           (condition-case nil
                               (json-parse-string line :object-type 'plist
                                                  :array-type 'list
                                                  :null-object nil
                                                  :false-object nil)
                             (json-error nil)))))
          (when entry
            (when-let* ((ts (pilish-agents--iso-ms (plist-get entry :timestamp))))
              (setq last-at ts))
            (pcase (plist-get entry :type)
              ("session" (setq sid (plist-get entry :id)
                               started (pilish-agents--iso-ms
                                        (plist-get entry :timestamp))))
              ("model_change" (setq model (plist-get entry :modelId)))
              ("session_info"
               (let ((name (plist-get entry :name)))
                 (if (and (null handle) (stringp name)
                          (string-match "\\`\\(.+\\)#\\([0-9a-f]\\{8\\}\\)\\'" name))
                     (setq handle (cons (match-string 1 name) (match-string 2 name)))
                   (setq title name))))
              ("message"
               (let* ((msg (plist-get entry :message))
                      (role (plist-get msg :role)))
                 (pcase role
                   ("user"
                    (unless prompt
                      (setq prompt (mapconcat (lambda (b) (or (plist-get b :text) ""))
                                              (pilish-agents--content-blocks msg) ""))))
                   ("assistant"
                    (cl-incf turns)
                    (setq stop (plist-get msg :stopReason))
                    (when-let* ((usage (plist-get msg :usage)))
                      (setq ctx (plist-get usage :totalTokens))
                      (when-let* ((c (plist-get (plist-get usage :cost) :total)))
                        (setq cost (+ cost c))))
                    (dolist (block (pilish-agents--content-blocks msg))
                      (pcase (plist-get block :type)
                        ("text"
                         (setq last-text (plist-get block :text))
                         (dolist (l (last (pilish-agents--text-lines last-text) 3))
                           (push (list :kind "text" :text l) recent)))
                        ("toolCall"
                         (cl-incf tools)
                         (push (list :kind "tool"
                                     :text (pilish-agents--tool-summary
                                            (plist-get block :name)
                                            (plist-get block :arguments)))
                               recent)))))))))))
        (forward-line 1)))
    (let* ((age (- (float-time) (float-time (file-attribute-modification-time
                                             (file-attributes file)))))
           (status (cond ((member stop '("error")) "error")
                         ((member stop '("aborted")) "aborted")
                         ((equal stop "stop") "completed")
                         ((< age pilish-agents-disk-running-seconds) "running")
                         (t "interrupted")))
           (desc (or title
                     (car (pilish-agents--text-lines prompt))
                     (file-name-base file))))
      (list :id (or (cdr handle) (and sid (substring sid 0 (min 8 (length sid)))))
            :type (or (car handle) "agent")
            :description (truncate-string-to-width desc 200)
            :prompt prompt
            :status status
            :model model
            :toolUses tools
            :turnCount turns
            :contextTokens ctx
            :cost (and (> cost 0) cost)
            :startedAt started
            :completedAt (unless (equal status "running") last-at)
            :activity (and (equal status "running")
                           (let ((tool (seq-find (lambda (r) (equal (plist-get r :kind) "tool"))
                                                 recent)))
                             (plist-get tool :text)))
            :recent (last (nreverse recent) 40)
            :resultPreview (and (equal status "completed") last-text)
            :sessionFile file
            :sessionId sid))))

(defun pilish-agents--disk-agents (parent-file)
  "Return agent plists for child sessions of PARENT-FILE."
  (when (and (stringp parent-file) (file-readable-p parent-file))
    (let* ((dir (file-name-directory parent-file))
           (parent (expand-file-name parent-file))
           (floor (file-name-nondirectory parent-file))
           agents)
      (dolist (file (directory-files dir t "\\.jsonl\\'" t))
        (when (string> (file-name-nondirectory file) floor)
          (let ((header (pilish-agents--file-header file)))
            (when (and (plistp header)
                       (stringp (plist-get header :parentSession))
                       (equal (expand-file-name (plist-get header :parentSession))
                              parent))
              (let* ((attrs (file-attributes file))
                     (stamp (list (file-attribute-modification-time attrs)
                                  (file-attribute-size attrs)))
                     (cached (gethash file pilish-agents--disk-cache)))
                (push (if (and cached (equal (car cached) stamp)
                               ;; Re-derive running/interrupted as time passes.
                               (not (equal (plist-get (cdr cached) :status) "running")))
                          (cdr cached)
                        (cdr (puthash file (cons stamp (pilish-agents--parse-child-session file))
                                      pilish-agents--disk-cache)))
                      agents))))))
      agents)))

;;;; Sources: demo

(defconst pilish-agents--demo-chatter
  '(("tool" . "read pilish-render.el")
    ("tool" . "rg -n setWidget src/ui")
    ("text" . "The widget factory is dropped over RPC; only string arrays survive.")
    ("tool" . "read src/agent-manager.ts")
    ("thinking" . "Checking whether the status line counts workflow children…")
    ("tool" . "ysh make test-render SELECTOR=extension-ui")
    ("text" . "Found 3 call sites that need the new accessor.")
    ("tool" . "edit pilish-ui.el")
    ("text" . "Tests pass locally; moving on to the header-line formatter.")
    ("tool" . "rg -n parentSession ~/.pi/agent/sessions"))
  "Canned recent-output lines for the demo fleet.")

(defun pilish-agents--demo-agent (id type desc status age-s &rest props)
  "Return a demo agent ID of TYPE with DESC and STATUS, AGE-S seconds old.
PROPS override defaults."
  (let* ((now (pilish-agents--now-ms))
         (start (- now (* 1000 age-s)))
         (base (list :id id :type type :description desc :status status
                     :background t :model "claude-sonnet-5" :thinking "medium"
                     :toolUses 0 :turnCount 0 :maxTurns nil :tokens 0
                     :cost 0.0 :startedAt start :completedAt nil
                     :activity nil :recent nil :resultPreview nil :error nil
                     :sessionFile nil :demoFinishAt nil)))
    (while props
      (setq base (plist-put base (pop props) (pop props))))
    base))

(defun pilish-agents--demo-recent (n)
  "Return N canned recent items."
  (mapcar (lambda (i)
            (let ((c (nth (mod i (length pilish-agents--demo-chatter))
                          pilish-agents--demo-chatter)))
              (list :kind (car c) :text (cdr c))))
          (number-sequence 0 (1- n))))

(defun pilish-agents--make-demo ()
  "Return a fresh demo fleet."
  (let ((now (pilish-agents--now-ms)))
    (list
     :agents
     (list
      (pilish-agents--demo-agent
       "a1b2c3d4e5f6a7b8c" "Explore" "Map pi-subagents widget surfaces" "running" 252
       :model "claude-haiku-4-5" :toolUses 31 :turnCount 14 :tokens 118400 :cost 0.09
       :activity "read src/ui/agent-widget.ts" :recent (pilish-agents--demo-recent 6)
       :demoFinishAt (+ now 240000))
      (pilish-agents--demo-agent
       "9f8e7d6c5b4a39281" "general-purpose" "Draft subagents:state publisher" "running" 128
       :name "feed" :toolUses 12 :turnCount 9 :maxTurns 30 :tokens 64200 :cost 0.31
       :activity "edit src/ui/agent-widget.ts" :recent (pilish-agents--demo-recent 4)
       :demoFinishAt (+ now 420000))
      (pilish-agents--demo-agent
       "5e5e5e5e5e5e5e5e5" "Plan" "Design /agents control verbs" "queued" 3
       :model "claude-opus-5" :thinking "high"
       :activity "queued — waiting for a background slot (1 ahead)")
      (pilish-agents--demo-agent
       "0c0ffee0c0ffee0c0" "general-purpose" "Port fake-pi widget scenario" "completed" 740
       :toolUses 22 :turnCount 11 :tokens 88100 :cost 0.42
       :completedAt (- now 520000) :recent (pilish-agents--demo-recent 5)
       :resultPreview "Added `widget_state` scenario to fake_pi.py emitting setWidget lines; contract doc updated.")
      (pilish-agents--demo-agent
       "beadfeedbeadfeedb" "Explore" "Find header-line status code" "steered" 1100
       :model "claude-haiku-4-5" :toolUses 9 :turnCount 5 :tokens 21000 :cost 0.01
       :completedAt (- now 1020000)
       :recent (append (pilish-agents--demo-recent 2)
                       (list (list :kind "steer" :text "Only look at pilish-ui.el")))
       :resultPreview "Status segments are formatted by pilish--header-format-extension-status (pilish-ui.el).")
      (pilish-agents--demo-agent
       "dead0000beef11112" "general-purpose" "Run make check on agents branch" "error" 400
       :toolUses 4 :turnCount 3 :tokens 9800 :cost 0.05 :completedAt (- now 340000)
       :error "make check exited 2: 3 tests failed in pilish-render-test.el"
       :recent (list (list :kind "tool" :text "ysh make check")
                     (list :kind "text" :text "3 unexpected results.")))
      (pilish-agents--demo-agent
       "77aa77aa77aa77aa7" "Explore" "Audit snapshot refresh path" "interrupted" 600
       :model "claude-haiku-4-5" :toolUses 15 :turnCount 7 :tokens 40100 :cost 0.03
       :completedAt (- now 480000) :recent (pilish-agents--demo-recent 3)))
     :workflows
     (list
      (list :id "wf_3f2a9c1b0e7d" :name "review-changes"
            :description "Review changed files across dimensions, verify each finding"
            :status "running" :startedAt (- now 190000) :pausedMs 0
            :phases (list (list :index 0 :title "Review") (list :index 1 :title "Verify")
                          (list :index 2 :title "Synthesize"))
            :phaseIndex 1 :totalTokens 412000 :totalToolCalls 133
            :scriptPath "/tmp/pi-subagents-501/…/tasks/wf_3f2a9c1b0e7d.workflow.js"
            :logs (list "3 dimensions reviewed, 4 findings" "verifying 4 findings")
            :agents
            (list (list :index 0 :label "review:bugs" :phaseIndex 0 :state "done" :toolCalls 21 :tokens 81000 :durationMs 38000)
                  (list :index 1 :label "review:perf" :phaseIndex 0 :state "done" :toolCalls 17 :tokens 64000 :durationMs 41000)
                  (list :index 2 :label "review:docs" :phaseIndex 0 :state "done" :toolCalls 9 :tokens 30000 :durationMs 22000)
                  (list :index 3 :label "verify:render.el" :phaseIndex 1 :state "running" :toolCalls 6 :tokens 20000 :durationMs 12000)
                  (list :index 4 :label "verify:ui.el" :phaseIndex 1 :state "running" :toolCalls 3 :tokens 9000 :durationMs 7000)
                  (list :index 5 :label "verify:menu.el" :phaseIndex 1 :state "queued" :toolCalls 0 :tokens 0 :durationMs 0)
                  (list :index 6 :label "verify:core.el" :phaseIndex 1 :state "queued" :toolCalls 0 :tokens 0 :durationMs 0)
                  (list :index 7 :label "synthesize" :phaseIndex 2 :state "queued" :toolCalls 0 :tokens 0 :durationMs 0)))
      (list :id "wf_8c41d07a22b9" :name "find-flaky-tests"
            :description "Find flaky tests and propose fixes"
            :status "completed" :startedAt (- now 1900000) :endedAt (- now 1500000)
            :pausedMs 0
            :phases (list (list :index 0 :title "Scan") (list :index 1 :title "Fix"))
            :phaseIndex 1 :totalTokens 230000 :totalToolCalls 71
            :logs (list "2 flaky tests found" "2 fixes proposed")
            :agents
            (list (list :index 0 :label "scan" :phaseIndex 0 :state "done" :toolCalls 30 :tokens 90000 :durationMs 120000)
                  (list :index 1 :label "fix:gui-resize" :phaseIndex 1 :state "done" :toolCalls 25 :tokens 80000 :durationMs 160000)
                  (list :index 2 :label "fix:timer-race" :phaseIndex 1 :state "skipped" :toolCalls 0 :tokens 0 :durationMs 0)))))))

(defun pilish-agents--demo-advance-agent (agent now)
  "Advance demo AGENT one tick at NOW."
  (pcase (plist-get agent :status)
    ("running"
     (when (zerop (random 2))
       (plist-put agent :toolUses (1+ (plist-get agent :toolUses))))
     (plist-put agent :tokens (+ (plist-get agent :tokens) (random 2500)))
     (plist-put agent :cost (+ (plist-get agent :cost) (/ (random 100) 10000.0)))
     (when (zerop (random 3))
       (let* ((c (nth (random (length pilish-agents--demo-chatter))
                      pilish-agents--demo-chatter))
              (item (list :kind (car c) :text (cdr c))))
         (plist-put agent :recent (last (append (plist-get agent :recent) (list item)) 40))
         (when (equal (car c) "tool")
           (plist-put agent :activity (cdr c))
           (plist-put agent :turnCount (1+ (plist-get agent :turnCount))))))
     (when-let* ((finish (plist-get agent :demoFinishAt)))
       (when (> now finish)
         (plist-put agent :status "completed")
         (plist-put agent :completedAt now)
         (plist-put agent :activity nil)
         (plist-put agent :resultPreview
                    "Done. Summary written; see recent output for details."))))))

(defun pilish-agents--demo-advance-workflow (wf now)
  "Advance demo workflow WF one tick at NOW."
  (when (equal (plist-get wf :status) "running")
    (let ((agents (plist-get wf :agents)))
      (plist-put wf :totalTokens (apply #'+ (mapcar (lambda (a) (plist-get a :tokens)) agents)))
      (plist-put wf :totalToolCalls (apply #'+ (mapcar (lambda (a) (plist-get a :toolCalls)) agents)))
      (dolist (a agents)
        (when (equal (plist-get a :state) "running")
          (plist-put a :durationMs (+ (plist-get a :durationMs) 1000))
          (plist-put a :tokens (+ (plist-get a :tokens) (random 3000)))
          (when (zerop (random 2))
            (plist-put a :toolCalls (1+ (plist-get a :toolCalls))))
          (when (zerop (random 30))
            (plist-put a :state "done"))))
      (let ((running (seq-count (lambda (a) (equal (plist-get a :state) "running")) agents))
            (phase (plist-get wf :phaseIndex)))
        (dolist (a agents)
          (when (and (< running 2) (equal (plist-get a :state) "queued")
                     (eql (plist-get a :phaseIndex) phase))
            (plist-put a :state "running")
            (cl-incf running)))
        (when (seq-every-p (lambda (a) (or (not (eql (plist-get a :phaseIndex) phase))
                                           (member (plist-get a :state) '("done" "skipped" "failed"))))
                           agents)
          (if (< phase (1- (length (plist-get wf :phases))))
              (progn (plist-put wf :phaseIndex (1+ phase))
                     (plist-put wf :logs (append (plist-get wf :logs)
                                                 (list (format "phase %d complete" (1+ phase))))))
            (plist-put wf :status "completed")
            (plist-put wf :endedAt now))))))
  (when (equal (plist-get wf :status) "paused")
    (plist-put wf :pausedMs (+ (or (plist-get wf :pausedMs) 0) 1000))))

(defun pilish-agents--demo-tick (demo)
  "Advance DEMO by one tick."
  (let ((now (pilish-agents--now-ms)))
    (dolist (a (plist-get demo :agents))
      (pilish-agents--demo-advance-agent a now))
    ;; Promote one queued agent when fewer than two run.
    (when (< (seq-count (lambda (a) (equal (plist-get a :status) "running"))
                        (plist-get demo :agents))
             2)
      (when-let* ((q (seq-find (lambda (a) (equal (plist-get a :status) "queued"))
                               (plist-get demo :agents))))
        (plist-put q :status "running")
        (plist-put q :startedAt now)
        (plist-put q :activity "read AGENTS.md")
        (plist-put q :demoFinishAt (+ now 300000))))
    (dolist (wf (plist-get demo :workflows))
      (pilish-agents--demo-advance-workflow wf now))))

;;;; View model

(defconst pilish-agents--all-controls
  '("steer" "interrupt" "stop" "resume" "spawn" "state"
    "workflow pause" "workflow resume" "workflow kill"
    "workflow skip" "workflow retry")
  "Every control verb the menu can send.")

(defconst pilish-agents--active-statuses '("running" "queued")
  "Agent statuses that count as active.")

(defun pilish-agents--compute-view ()
  "Compute the view plist for the current chat buffer."
  (let ((feed (and (not pilish-agents--demo) (pilish-agents--feed-state))))
    (cond
     (pilish-agents--demo
      (list :source 'demo
            :agents (plist-get pilish-agents--demo :agents)
            :workflows (plist-get pilish-agents--demo :workflows)
            :controls pilish-agents--all-controls))
     (feed
      (list :source 'feed
            :agents (seq-remove (lambda (a) (pilish-agents--get a :workflowId))
                                (plist-get feed :agents))
            :workflows (plist-get feed :workflows)
            :controls (plist-get feed :controls)
            :types (plist-get feed :types)))
     (t
      (list :source 'disk
            :agents (pilish-agents--disk-agents (plist-get pilish--state :session-file))
            :workflows nil
            :controls nil)))))

(defun pilish-agents--sort (agents)
  "Return AGENTS with active first, then newest first."
  (sort (copy-sequence agents)
        (lambda (a b)
          (let ((aa (member (plist-get a :status) pilish-agents--active-statuses))
                (ba (member (plist-get b :status) pilish-agents--active-statuses)))
            (if (not (eq (not aa) (not ba)))
                aa
              (> (or (pilish-agents--get a :completedAt) (pilish-agents--get a :startedAt) 0)
                 (or (pilish-agents--get b :completedAt) (pilish-agents--get b :startedAt) 0)))))))

(defun pilish-agents--filter ()
  "Return the session's current agent list filter."
  (buffer-local-value 'pilish-agents--filter (pilish-agents--chat)))

(defun pilish-agents--filtered-agents (view)
  "Return VIEW's agents after the session's current filter."
  (let* ((sorted (pilish-agents--sort (plist-get view :agents)))
         (active (seq-filter (lambda (a) (member (plist-get a :status)
                                                 pilish-agents--active-statuses))
                             sorted))
         (finished (seq-remove (lambda (a) (member (plist-get a :status)
                                                   pilish-agents--active-statuses))
                               sorted)))
    (pcase (pilish-agents--filter)
      ('active active)
      ('all sorted)
      (_ (append active (seq-take finished pilish-agents-recent-finished))))))

(defun pilish-agents--items (view)
  "Return VIEW's selectable items as (KIND . PLIST), in display order."
  (append (mapcar (lambda (a) (cons 'agent a)) (pilish-agents--filtered-agents view))
          (mapcar (lambda (w) (cons 'workflow w))
                  (if (eq (pilish-agents--filter) 'active)
                      (seq-filter (lambda (w) (member (plist-get w :status) '("running" "paused")))
                                  (plist-get view :workflows))
                    (plist-get view :workflows)))))

(defun pilish-agents--item-key (item)
  "Return the selection key of ITEM."
  (cons (car item) (plist-get (cdr item) :id)))

(defun pilish-agents--refresh ()
  "Recompute the view in the chat buffer and fix up the selection."
  (pilish-agents--in-chat
    (setq pilish-agents--view (pilish-agents--compute-view))
    (let ((items (pilish-agents--items pilish-agents--view)))
      (unless (seq-find (lambda (it) (equal (pilish-agents--item-key it)
                                            pilish-agents--selected))
                        items)
        (setq pilish-agents--selected
              (and items (pilish-agents--item-key (car items))))))
    pilish-agents--view))

(defun pilish-agents--view ()
  "Return the current view, computing it when absent."
  (pilish-agents--in-chat (or pilish-agents--view (pilish-agents--refresh))))

(defun pilish-agents--selected-item ()
  "Return the selected (KIND . PLIST), or nil."
  (pilish-agents--in-chat
    (seq-find (lambda (it) (equal (pilish-agents--item-key it) pilish-agents--selected))
              (pilish-agents--items (pilish-agents--view)))))

(defun pilish-agents--selected-agent ()
  "Return the selected agent plist, or nil."
  (let ((it (pilish-agents--selected-item)))
    (and (eq (car it) 'agent) (cdr it))))

(defun pilish-agents--selected-workflow ()
  "Return the selected workflow plist, or nil."
  (let ((it (pilish-agents--selected-item)))
    (and (eq (car it) 'workflow) (cdr it))))

;;;; Formatting

(defconst pilish-agents--spinner ["◐" "◓" "◑" "◒"]
  "Spinner frames for running items.")

(defun pilish-agents--status-face (status)
  "Return the face for STATUS."
  (pcase status
    ("running" 'pilish-agents-running)
    ((or "queued" "blocked") 'pilish-agents-queued)
    ((or "completed" "steered" "done") 'pilish-agents-done)
    ((or "error" "failed") 'pilish-agents-failed)
    (_ 'pilish-agents-halted)))

(defun pilish-agents--glyph (status)
  "Return a propertized one-column glyph for STATUS."
  (propertize
   (pcase status
     ("running" (aref pilish-agents--spinner (mod pilish-agents--tick 4)))
     ((or "queued" "blocked") "◌")
     ((or "completed" "steered" "done") "✓")
     ((or "error" "failed") "✗")
     ((or "interrupted" "paused") "◇")
     ((or "stopped" "aborted" "killed") "■")
     ("skipped" "»")
     (_ "○"))
   'face (pilish-agents--status-face status)))

(defun pilish-agents--duration (ms)
  "Format MS milliseconds compactly."
  (if (not (numberp ms)) ""
    (let ((s (/ (max 0 ms) 1000)))
      (cond ((< s 60) (format "%ds" s))
            ((< s 3600) (format "%dm%02ds" (/ s 60) (% s 60)))
            (t (format "%dh%02dm" (/ s 3600) (% (/ s 60) 60)))))))

(defun pilish-agents--ago (ms)
  "Format epoch MS as a relative age."
  (if (numberp ms)
      (concat (pilish-agents--duration (- (pilish-agents--now-ms) ms)) " ago")
    ""))

(defun pilish-agents--tokens (n)
  "Format token count N."
  (cond ((not (numberp n)) "")
        ((>= n 1000000) (format "%.1fM" (/ n 1000000.0)))
        ((>= n 1000) (format "%.0fk" (/ n 1000.0)))
        (t (format "%d" n))))

(defun pilish-agents--elapsed (item &optional end-key)
  "Return ITEM's elapsed milliseconds, ending at END-KEY or now.
A paused workflow's clock stops at its `pausedAt'."
  (when-let* ((start (pilish-agents--get item :startedAt)))
    (- (or (pilish-agents--get item (or end-key :completedAt))
           (pilish-agents--get item :pausedAt)
           (pilish-agents--now-ms))
       start
       (or (pilish-agents--get item :pausedMs) 0))))

(defun pilish-agents--pad (string width &optional right)
  "Truncate or pad STRING to WIDTH columns; RIGHT aligns right."
  (let ((s (truncate-string-to-width (or string "") width nil nil "…")))
    (if right
        (concat (make-string (max 0 (- width (string-width s))) ?\s) s)
      (concat s (make-string (max 0 (- width (string-width s))) ?\s)))))

(defun pilish-agents--desc-width ()
  "Return the description column width for the current frame."
  (max 18 (min 52 (- (frame-width) 72))))

(defun pilish-agents--label (agent)
  "Return AGENT's short label: its name, handle, or type."
  (or (pilish-agents--get agent :name) (pilish-agents--get agent :handle)
      (pilish-agents--get agent :type) "agent"))

(defun pilish-agents--short-id (id)
  "Return the first 8 characters of ID."
  (if (stringp id) (substring id 0 (min 8 (length id))) ""))

(defun pilish-agents--agent-row (agent selected)
  "Return the menu row for AGENT; SELECTED highlights it."
  (let* ((status (plist-get agent :status))
         (finished (not (member status pilish-agents--active-statuses)))
         (row (concat
               (pilish-agents--glyph status) " "
               (propertize (pilish-agents--pad (pilish-agents--label agent) 18)
                           'face 'pilish-agents-type)
               " "
               (propertize (pilish-agents--pad (plist-get agent :description)
                                               (pilish-agents--desc-width))
                           'face (and finished 'pilish-agents-meta))
               " "
               (propertize (pilish-agents--pad status 12)
                           'face (pilish-agents--status-face status))
               (propertize
                (concat
                 (pilish-agents--pad (pilish-agents--duration
                                      (pilish-agents--elapsed agent))
                                     7 t)
                 (pilish-agents--pad (format "%s tools" (or (pilish-agents--get agent :toolUses) 0))
                                     10 t)
                 (pilish-agents--pad (pilish-agents--tokens
                                      (or (pilish-agents--get agent :tokens)
                                          (pilish-agents--get agent :contextTokens)))
                                     7 t))
                'face 'pilish-agents-meta))))
    (pilish-agents--mark-selected row selected)))

(defun pilish-agents--mark-selected (row selected)
  "Return ROW with a selection marker, highlighted when SELECTED."
  (if selected
      (let ((r (copy-sequence row)))
        (add-face-text-property 0 (length r) 'pilish-agents-selected t r)
        (concat (propertize "▸" 'face 'pilish-agents-activity) r))
    (concat " " row)))

(defun pilish-agents--workflow-row (wf selected)
  "Return the menu row for workflow WF; SELECTED highlights it."
  (let* ((status (plist-get wf :status))
         (phases (plist-get wf :phases))
         (phase (nth (or (pilish-agents--get wf :phaseIndex) 0) phases))
         (agents (plist-get wf :agents))
         (done (seq-count (lambda (a) (member (plist-get a :state) '("done" "skipped")))
                          agents))
         (row (concat
               (pilish-agents--glyph status) " "
               (propertize (pilish-agents--pad (plist-get wf :name) 18)
                           'face 'pilish-agents-type)
               " "
               (pilish-agents--pad
                (format "phase %d/%d %s · %d/%d agents"
                        (1+ (or (pilish-agents--get wf :phaseIndex) 0)) (length phases)
                        (or (plist-get phase :title) "") done (length agents))
                (pilish-agents--desc-width))
               " "
               (propertize (pilish-agents--pad status 12)
                           'face (pilish-agents--status-face status))
               (propertize
                (concat (pilish-agents--pad (pilish-agents--duration
                                             (pilish-agents--elapsed wf :endedAt))
                                            7 t)
                        (pilish-agents--pad (format "%s tools"
                                                    (or (pilish-agents--get wf :totalToolCalls) 0))
                                            10 t)
                        (pilish-agents--pad (pilish-agents--tokens
                                             (pilish-agents--get wf :totalTokens))
                                            7 t))
                'face 'pilish-agents-meta))))
    (pilish-agents--mark-selected row selected)))

(defun pilish-agents--rule (title)
  "Return a horizontal rule carrying TITLE."
  (let* ((width (min 110 (- (frame-width) 4)))
         (head (concat "── " title " ")))
    (concat (propertize head 'face 'pilish-agents-heading)
            (propertize (make-string (max 3 (- width (string-width head))) ?─)
                        'face 'pilish-agents-rule))))

(defun pilish-agents--panel-line (text &optional face)
  "Return detail panel line TEXT with optional FACE."
  (concat (propertize "  │ " 'face 'pilish-agents-rule)
          (if face (propertize text 'face face) text)))

(defun pilish-agents--recent-line (item &optional width)
  "Format recent output ITEM, truncated to WIDTH when non-nil."
  (let* ((raw (replace-regexp-in-string "[\n\t]+" " " (or (plist-get item :text) "")))
         (text (if width (pilish-agents--pad raw width) raw)))
    (pcase (plist-get item :kind)
      ("tool" (propertize (concat "→ " text) 'face 'pilish-agents-tool))
      ("steer" (propertize (concat "⇢ steer: " text) 'face 'pilish-agents-steer))
      ("thinking" (propertize (concat "  " text) 'face 'pilish-agents-meta))
      ("status" (propertize (concat "• " text) 'face 'pilish-agents-halted))
      (_ (concat "  " text)))))

(defun pilish-agents--join-meta (&rest parts)
  "Join non-empty PARTS with a middle dot."
  (mapconcat #'identity (seq-remove (lambda (p) (or (null p) (string-empty-p p))) parts)
             (propertize " · " 'face 'pilish-agents-meta)))

(defun pilish-agents--agent-detail (agent)
  "Return the detail panel for AGENT."
  (let* ((status (plist-get agent :status))
         (width (max 30 (min 100 (- (frame-width) 12))))
         (turns (pilish-agents--get agent :turnCount))
         (max-turns (pilish-agents--get agent :maxTurns))
         (cost (pilish-agents--get agent :cost))
         (lines
          (list
           (pilish-agents--rule
            (format "%s · %s" (pilish-agents--label agent)
                    (pilish-agents--short-id (plist-get agent :id))))
           (pilish-agents--panel-line
            (pilish-agents--pad (plist-get agent :description) width)
            'pilish-agents-title)
           (pilish-agents--panel-line
            (pilish-agents--join-meta
             (concat (pilish-agents--glyph status) " "
                     (propertize status 'face (pilish-agents--status-face status)))
             (pilish-agents--duration (pilish-agents--elapsed agent))
             (and (pilish-agents--get agent :completedAt)
                  (concat "ended " (pilish-agents--ago (pilish-agents--get agent :completedAt))))
             (and turns (if max-turns (format "turn %d/%d" turns max-turns) (format "%d turns" turns)))
             (format "%d tools" (or (pilish-agents--get agent :toolUses) 0))
             (let ((tok (or (pilish-agents--get agent :tokens)
                            (pilish-agents--get agent :contextTokens))))
               (and tok (concat (pilish-agents--tokens tok) " tok")))
             (and (numberp cost) (> cost 0) (format "$%.2f" cost))
             (and (pilish-agents--get agent :name) (pilish-agents--get agent :type))
             (pilish-agents--get agent :model)
             (pilish-agents--get agent :thinking))))))
    (when-let* ((activity (pilish-agents--get agent :activity)))
      (setq lines (append lines (list (pilish-agents--panel-line
                                       (concat "⟳ " (pilish-agents--pad activity width))
                                       'pilish-agents-activity)))))
    (when-let* ((err (pilish-agents--get agent :error)))
      (setq lines (append lines (list (pilish-agents--panel-line
                                       (concat "✗ " (pilish-agents--pad err width))
                                       'pilish-agents-failed)))))
    (let ((recent (last (pilish-agents--get agent :recent) pilish-agents-recent-lines)))
      (if (and (not recent) (not (pilish-agents--get agent :resultPreview)))
          (setq lines (append lines (list (pilish-agents--panel-line "no output yet"
                                                                     'pilish-agents-meta))))
        (dolist (r recent)
          (setq lines (append lines (list (pilish-agents--panel-line
                                           (pilish-agents--recent-line r width))))))))
    (when-let* ((result (pilish-agents--get agent :resultPreview)))
      (setq lines (append lines
                          (list (pilish-agents--panel-line
                                 (concat (propertize "result " 'face 'pilish-agents-done)
                                         (pilish-agents--pad
                                          (car (pilish-agents--text-lines result))
                                          (- width 7))))))))
    (string-join lines "\n")))

(defconst pilish-agents--workflow-step-limit 9
  "Most workflow steps listed in the detail panel.")

(defun pilish-agents--workflow-visible-steps (steps)
  "Return the STEPS to list: all unfinished steps, then the latest finished.
Keep display order and at most `pilish-agents--workflow-step-limit'."
  (if (<= (length steps) pilish-agents--workflow-step-limit)
      steps
    (let* ((finished-p (lambda (a) (member (plist-get a :state) '("done" "skipped"))))
           (open (seq-remove finished-p steps))
           (room (max 0 (- pilish-agents--workflow-step-limit (length open))))
           (keep (append (last (seq-filter finished-p steps) room) open)))
      (seq-take (seq-filter (lambda (a) (memq a keep)) steps)
                pilish-agents--workflow-step-limit))))

(defun pilish-agents--workflow-step-line (step)
  "Return the detail line for workflow STEP."
  (let* ((state (plist-get step :state))
         (started (not (member state '("queued" "blocked")))))
    (concat
     (pilish-agents--pad (format "%d" (plist-get step :index)) 2 t) " "
     (pilish-agents--glyph state) " "
     (pilish-agents--pad (plist-get step :label) 22) " "
     (propertize (pilish-agents--pad state 12) 'face (pilish-agents--status-face state))
     (propertize
      (if (not started) ""
        (concat (pilish-agents--pad (pilish-agents--duration
                                     (pilish-agents--get step :durationMs))
                                    7 t)
                (pilish-agents--pad (format "%s tools" (or (pilish-agents--get step :toolCalls) 0))
                                    10 t)
                (pilish-agents--pad (pilish-agents--tokens (pilish-agents--get step :tokens))
                                    7 t)))
      'face 'pilish-agents-meta)
     (if-let* ((err (pilish-agents--get step :error)))
         (propertize (concat "  " (truncate-string-to-width err 50 nil nil "…"))
                     'face 'pilish-agents-failed)
       ""))))

(defun pilish-agents--workflow-detail (wf)
  "Return the detail panel for workflow WF."
  (let* ((width (max 30 (min 100 (- (frame-width) 12))))
         (status (plist-get wf :status))
         (phase-index (or (pilish-agents--get wf :phaseIndex) 0))
         (agents (plist-get wf :agents))
         (lines
          (list
           (pilish-agents--rule (format "%s · %s" (plist-get wf :name) (plist-get wf :id)))
           (pilish-agents--panel-line
            (pilish-agents--pad (or (pilish-agents--get wf :description) "") width)
            'pilish-agents-title)
           (pilish-agents--panel-line
            (pilish-agents--join-meta
             (concat (pilish-agents--glyph status) " "
                     (propertize status 'face (pilish-agents--status-face status)))
             (pilish-agents--duration (pilish-agents--elapsed wf :endedAt))
             (format "%d/%d agents"
                     (seq-count (lambda (a) (equal (plist-get a :state) "done")) agents)
                     (length agents))
             (format "%d tools" (or (pilish-agents--get wf :totalToolCalls) 0))
             (concat (pilish-agents--tokens (pilish-agents--get wf :totalTokens)) " tok")))
           (pilish-agents--panel-line
            (mapconcat
             (lambda (p)
               (let ((i (plist-get p :index)))
                 (cond ((< i phase-index)
                        (concat (propertize "✓ " 'face 'pilish-agents-done) (plist-get p :title)))
                       ((= i phase-index)
                        (concat (pilish-agents--glyph (if (equal status "running") "running" status))
                                " " (propertize (plist-get p :title) 'face 'bold)))
                       (t (propertize (concat "○ " (plist-get p :title)) 'face 'pilish-agents-meta)))))
             (plist-get wf :phases)
             (propertize "  ›  " 'face 'pilish-agents-rule))))))
    (let* ((shown (pilish-agents--workflow-visible-steps agents))
           (hidden (- (length agents) (length shown))))
      (when (> hidden 0)
        (setq lines (append lines (list (pilish-agents--panel-line
                                         (format "   … %d earlier step%s finished"
                                                 hidden (if (= hidden 1) "" "s"))
                                         'pilish-agents-meta)))))
      (dolist (a shown)
        (setq lines (append lines (list (pilish-agents--panel-line
                                         (pilish-agents--workflow-step-line a)))))))
    (when-let* ((log (car (last (pilish-agents--get wf :logs)))))
      (setq lines (append lines (list (pilish-agents--panel-line
                                       (concat "log: " (pilish-agents--pad log width))
                                       'pilish-agents-meta)))))
    (string-join lines "\n")))

(defun pilish-agents--count (agents status)
  "Return how many AGENTS have STATUS (a string or list)."
  (seq-count (lambda (a) (member (plist-get a :status)
                                 (if (listp status) status (list status))))
             agents))

(defun pilish-agents--source-badge (source)
  "Return a header badge for SOURCE."
  (pcase source
    ('demo (propertize " DEMO · stub data " 'face 'pilish-agents-badge))
    ('disk (propertize "read-only · child sessions on disk" 'face 'pilish-agents-halted))
    ('feed (propertize "live · subagents:state" 'face 'pilish-agents-running))))

(defun pilish-agents--header (&rest _)
  "Return the agents menu header."
  (let* ((view (pilish-agents--view))
         (agents (plist-get view :agents))
         (wfs (plist-get view :workflows))
         (name (truncate-string-to-width
                (pilish-agents--in-chat
                  (or pilish--session-name
                      (abbreviate-file-name default-directory)))
                40 nil nil "…"))
         (parts
          (seq-remove
           #'null
           (list
            (let ((n (pilish-agents--count agents "running")))
              (and (> n 0) (propertize (format "%s %d running" (pilish-agents--glyph "running") n)
                                       'face 'pilish-agents-running)))
            (let ((n (pilish-agents--count agents "queued")))
              (and (> n 0) (propertize (format "◌ %d queued" n) 'face 'pilish-agents-queued)))
            (let ((n (pilish-agents--count agents '("completed" "steered"))))
              (and (> n 0) (propertize (format "✓ %d done" n) 'face 'pilish-agents-done)))
            (let ((n (pilish-agents--count agents '("error"))))
              (and (> n 0) (propertize (format "✗ %d failed" n) 'face 'pilish-agents-failed)))
            (let ((n (pilish-agents--count agents '("interrupted" "stopped" "aborted"))))
              (and (> n 0) (propertize (format "◇ %d halted" n) 'face 'pilish-agents-halted)))
            (and wfs (format "◆ %d workflow%s" (length wfs) (if (cdr wfs) "s" "")))))))
    (concat (propertize "Subagents" 'face 'pilish-agents-heading)
            (propertize "  ·  " 'face 'pilish-agents-rule)
            (propertize name 'face 'pilish-agents-meta)
            (propertize "  ·  " 'face 'pilish-agents-rule)
            (if parts (string-join parts "   ")
              (propertize "nothing yet" 'face 'pilish-agents-meta))
            "    "
            (pilish-agents--source-badge (plist-get view :source))
            (pcase (plist-get view :source)
              ('disk (concat "\n" (propertize
                                   "Controls are stubs until pi-subagents publishes `subagents:state'.  D shows a demo fleet."
                                   'face 'pilish-agents-meta)))
              (_ "")))))

(defun pilish-agents--detail (&rest _)
  "Return the detail panel for the selected item."
  (let ((it (pilish-agents--selected-item)))
    (pcase (car it)
      ('agent (pilish-agents--agent-detail (cdr it)))
      ('workflow (pilish-agents--workflow-detail (cdr it)))
      (_ (concat (pilish-agents--rule "nothing selected") "\n"
                 (pilish-agents--panel-line
                  "No subagents in this session.  N spawns one; D shows a demo fleet."
                  'pilish-agents-meta))))))

;;;; Transient children

(defconst pilish-agents--item-keys
  '("1" "2" "3" "4" "5" "6" "7" "8" "9" "0")
  "Keys for directly selecting list items.")

(defun pilish-agents--visible-items ()
  "Return up to ten items around the selection as (INDEX . ITEM)."
  (pilish-agents--in-chat
    (let* ((items (pilish-agents--items (pilish-agents--view)))
           (pos (or (seq-position items pilish-agents--selected
                                  (lambda (it key) (equal (pilish-agents--item-key it) key)))
                    0))
           (start (max 0 (min (- pos 4) (- (length items) 10)))))
      (seq-map-indexed (lambda (it i) (cons (+ start i) it))
                       (seq-subseq items start (min (length items) (+ start 10)))))))

(defun pilish-agents--suffix-signature ()
  "Return what the menu's suffixes depend on beyond row text.
The visible item keys, the selection, and the selected item's status."
  (let ((selected (pilish-agents--selected-item)))
    (list (mapcar (lambda (e) (pilish-agents--item-key (cdr e)))
                  (pilish-agents--visible-items))
          (and selected (pilish-agents--item-key selected))
          (plist-get (cdr selected) :status))))

(defun pilish-agents--row-for-key (key)
  "Return the current menu row for item KEY, or an empty string."
  (pilish-agents--in-chat
    (let ((it (seq-find (lambda (it) (equal (pilish-agents--item-key it) key))
                        (pilish-agents--items (pilish-agents--view))))
          (selected (equal key pilish-agents--selected)))
      (pcase (car it)
        ('agent (pilish-agents--agent-row (cdr it) selected))
        ('workflow (pilish-agents--workflow-row (cdr it) selected))
        (_ "")))))

(defun pilish-agents--make-select-command (key)
  "Return a command selecting item KEY."
  (lambda ()
    (interactive)
    (pilish-agents--in-chat (setq pilish-agents--selected key))))

(defun pilish-agents--children (kind)
  "Return transient suffixes for visible items of KIND."
  (let* ((visible (pilish-agents--visible-items))
         (n 0) specs)
    (dolist (entry visible)
      (let* ((it (cdr entry))
             (key (pilish-agents--item-key it)))
        (when (eq (car it) kind)
          ;; Rows are functions so the live redisplay updates counters.
          (push (list (nth n pilish-agents--item-keys)
                      (pilish-agents--make-select-command key)
                      :description (lambda () (pilish-agents--row-for-key key))
                      :transient t)
                specs))
        (cl-incf n)))
    ;; Keys follow display order across both groups.
    (setq specs (nreverse specs))
    (setf (alist-get 'signature pilish-agents--rendered-keys)
          (pilish-agents--suffix-signature))
    (when (and (eq kind 'agent) (null specs))
      (setq specs (list (list :info (propertize "  (none)" 'face 'pilish-agents-meta)))))
    (transient-parse-suffixes 'pilish-agents-menu specs)))

(defun pilish-agents--hidden-count ()
  "Return how many items the visible window omits."
  (pilish-agents--in-chat
    (max 0 (- (length (pilish-agents--items (pilish-agents--view))) 10))))

(defun pilish-agents--agents-heading (&rest _)
  "Return the Agents group heading."
  (concat (propertize "Agents" 'face 'pilish-agents-heading)
          (propertize (format "  [%s]" (pilish-agents--in-chat pilish-agents--filter))
                      'face 'pilish-agents-meta)
          (let ((hidden (pilish-agents--hidden-count)))
            (if (> hidden 0)
                (propertize (format "  %d more · n/p scrolls" hidden) 'face 'pilish-agents-meta)
              ""))))

(defun pilish-agents--has-workflows-p ()
  "Return non-nil when there are workflows to show."
  (seq-find (lambda (it) (eq (car it) 'workflow))
            (mapcar #'cdr (pilish-agents--visible-items))))

;;;; Predicates

(defun pilish-agents--supports-p (verb)
  "Return non-nil when the current source accepts VERB."
  (member verb (plist-get (pilish-agents--view) :controls)))

(defun pilish-agents--allowed-p (verb)
  "Return non-nil unless a live feed omits VERB from its controls.
Stub sources keep every control usable so the menu can be explored."
  (let ((view (pilish-agents--view)))
    (or (not (eq (plist-get view :source) 'feed))
        (member verb (plist-get view :controls)))))

(defun pilish-agents--agent-status-p (statuses)
  "Return non-nil when the selected agent's status is in STATUSES."
  (when-let* ((a (pilish-agents--selected-agent)))
    (member (plist-get a :status) statuses)))

(defun pilish-agents--active-agent-p ()
  "Return non-nil when the selected agent is running or queued."
  (pilish-agents--agent-status-p '("running" "queued")))

(defun pilish-agents--can-steer-p ()
  "Return non-nil when the selected agent can be steered."
  (and (pilish-agents--active-agent-p) (pilish-agents--allowed-p "steer")))

(defun pilish-agents--can-interrupt-p ()
  "Return non-nil when the selected agent can be interrupted."
  (and (pilish-agents--active-agent-p) (pilish-agents--allowed-p "interrupt")))

(defun pilish-agents--can-stop-p ()
  "Return non-nil when the selected agent can be stopped."
  (and (pilish-agents--active-agent-p) (pilish-agents--allowed-p "stop")))

(defun pilish-agents--can-spawn-p ()
  "Return non-nil when new agents can be spawned."
  (pilish-agents--allowed-p "spawn"))

(defun pilish-agents--can-resume-p ()
  "Return non-nil when the selected agent can be resumed."
  (and (pilish-agents--agent-status-p
        '("completed" "steered" "interrupted" "stopped" "aborted" "error"))
       (pilish-agents--allowed-p "resume")))

(defun pilish-agents--has-session-p ()
  "Return non-nil when the selected item has a saved conversation."
  (or (and-let* ((a (pilish-agents--selected-agent)))
        (pilish-agents--get a :sessionFile))
      (and-let* ((w (pilish-agents--selected-workflow)))
        (seq-find (lambda (a) (pilish-agents--get a :sessionFile)) (plist-get w :agents)))))

(defun pilish-agents--workflow-status-p (statuses)
  "Return non-nil when the selected workflow's status is in STATUSES."
  (when-let* ((w (pilish-agents--selected-workflow)))
    (member (plist-get w :status) statuses)))

(defun pilish-agents--workflow-live-p ()
  "Return non-nil when the selected workflow is running or paused."
  (pilish-agents--workflow-status-p '("running" "paused")))

(defun pilish-agents--can-pause-p ()
  "Return non-nil when the selected workflow can be paused or resumed."
  (and (pilish-agents--workflow-live-p)
       (pilish-agents--allowed-p
        (if (pilish-agents--workflow-status-p '("paused"))
            "workflow resume" "workflow pause"))))

(defun pilish-agents--can-kill-p ()
  "Return non-nil when the selected workflow can be killed."
  (and (pilish-agents--workflow-live-p) (pilish-agents--allowed-p "workflow kill")))

(defun pilish-agents--can-skip-p ()
  "Return non-nil when a step of the selected workflow can be skipped."
  (and (pilish-agents--workflow-live-p) (pilish-agents--allowed-p "workflow skip")))

(defun pilish-agents--can-retry-p ()
  "Return non-nil when a step of the selected workflow can be retried."
  (and (pilish-agents--selected-workflow) (pilish-agents--allowed-p "workflow retry")))

;;;; Control dispatch

(defun pilish-agents--command-text (verb args)
  "Return the slash command text for VERB and ARGS."
  (format "/%s %s" pilish-agents-control-command
          (string-join (cons verb (delq nil args)) " ")))

(defun pilish-agents--send-control (text)
  "Send control command TEXT to the session's pi process.
A bare `prompt' request rather than `pilish-run-command': pi runs
extension commands immediately even while a turn streams, and the prompt
path would mark the session as sending and later idle over the live turn.
The extension reports the outcome with a notification."
  (let ((proc (pilish-agents--in-chat pilish--process)))
    (unless (process-live-p proc)
      (user-error "No live pi process for this session"))
    (pilish--rpc-async
     proc (list :type "prompt" :message text)
     (lambda (response)
       (unless (eq (plist-get response :success) t)
         (message "Pi: %s failed: %s" text
                  (or (plist-get response :error) "unknown error")))))))

(defun pilish-agents--request (verb &rest args)
  "Send control VERB with ARGS through the current source."
  (let* ((view (pilish-agents--view))
         (text (pilish-agents--command-text verb args))
         (capability (if (equal verb "workflow") (concat verb " " (car args)) verb)))
    (pcase (plist-get view :source)
      ('demo
       (pilish-agents--in-chat (pilish-agents--demo-apply verb args))
       (message "[stub] would send: %s" (truncate-string-to-width text 120 nil nil "…")))
      ('feed
       (if (pilish-agents--supports-p capability)
           (pilish-agents--send-control text)
         (message "pi-subagents does not advertise `%s'; not sent" capability)))
      (_
       (message "[stub] not sent (pi-subagents has no control feed yet): %s"
                (truncate-string-to-width text 120 nil nil "…"))))
    (pilish-agents--refresh)))

(defun pilish-agents--demo-find (id)
  "Return the demo agent or workflow with ID."
  (or (seq-find (lambda (a) (equal (plist-get a :id) id)) (plist-get pilish-agents--demo :agents))
      (seq-find (lambda (w) (equal (plist-get w :id) id)) (plist-get pilish-agents--demo :workflows))))

(defun pilish-agents--demo-push (agent kind text)
  "Append a recent item of KIND with TEXT to demo AGENT."
  (plist-put agent :recent (append (plist-get agent :recent)
                                   (list (list :kind kind :text text)))))

(defun pilish-agents--demo-apply (verb args)
  "Apply control VERB with ARGS to the demo fleet."
  (let* ((now (pilish-agents--now-ms))
         (target (pilish-agents--demo-find (car args))))
    (pcase verb
      ("steer" (pilish-agents--demo-push target "steer" (cadr args)))
      ("interrupt" (plist-put target :status "interrupted")
       (plist-put target :completedAt now) (plist-put target :activity nil)
       (pilish-agents--demo-push target "status" "interrupted — resumable"))
      ("stop" (plist-put target :status "stopped")
       (plist-put target :completedAt now) (plist-put target :activity nil)
       (pilish-agents--demo-push target "status" "stopped"))
      ("resume" (plist-put target :status "running")
       (plist-put target :completedAt nil) (plist-put target :error nil)
       (plist-put target :demoFinishAt (+ now 40000))
       (plist-put target :activity "resuming")
       (pilish-agents--demo-push target "steer" (concat "resume: " (cadr args))))
      ("spawn"
       (let ((agent (pilish-agents--demo-agent
                     (substring (md5 (format "%s%s" now (random))) 0 17)
                     (car args) (truncate-string-to-width (cadr args) 60) "queued" 0
                     :activity "queued — waiting for a background slot")))
         (plist-put pilish-agents--demo :agents
                    (append (plist-get pilish-agents--demo :agents) (list agent)))
         (setq pilish-agents--selected (cons 'agent (plist-get agent :id)))))
      ("workflow"
       (let* ((op (car args))
              (wf (pilish-agents--demo-find (cadr args)))
              (index (and (nth 2 args) (string-to-number (nth 2 args))))
              (step (and index (seq-find (lambda (a) (eql (plist-get a :index) index))
                                         (plist-get wf :agents)))))
         (pcase op
           ("pause" (plist-put wf :status "paused"))
           ("resume" (plist-put wf :status "running"))
           ("kill" (plist-put wf :status "killed") (plist-put wf :endedAt now)
            (dolist (a (plist-get wf :agents))
              (when (member (plist-get a :state) '("running" "queued"))
                (plist-put a :state "interrupted"))))
           ("skip" (when step (plist-put step :state "skipped")))
           ("retry" (when step (plist-put step :state "queued")
                          (plist-put step :toolCalls 0) (plist-put step :tokens 0)
                          (plist-put step :durationMs 0)))))))))

;;;; Commands

(defun pilish-agents--require-agent ()
  "Return the selected agent or signal."
  (or (pilish-agents--selected-agent) (user-error "Select an agent first")))

(defun pilish-agents--require-workflow ()
  "Return the selected workflow or signal."
  (or (pilish-agents--selected-workflow) (user-error "Select a workflow first")))

(defun pilish-agents-steer (message)
  "Steer the selected agent with MESSAGE."
  (interactive
   (let ((a (pilish-agents--require-agent)))
     (list (read-string (format "Steer %s (%s): " (pilish-agents--label a)
                                (truncate-string-to-width (plist-get a :description) 40
                                                          nil nil "…"))))))
  (unless (string-blank-p message)
    (pilish-agents--request "steer" (plist-get (pilish-agents--require-agent) :id) message)))

(defun pilish-agents-interrupt ()
  "Interrupt the selected agent, keeping it resumable."
  (interactive)
  (pilish-agents--request "interrupt" (plist-get (pilish-agents--require-agent) :id)))

(defun pilish-agents-stop ()
  "Stop the selected agent after confirmation."
  (interactive)
  (let ((a (pilish-agents--require-agent)))
    (when (y-or-n-p (format "Stop %s? " (pilish-agents--label a)))
      (pilish-agents--request "stop" (plist-get a :id)))))

(defun pilish-agents-resume (prompt)
  "Resume the selected agent with PROMPT."
  (interactive
   (let ((a (pilish-agents--require-agent)))
     (list (read-string (format "Resume %s with: " (pilish-agents--label a))))))
  (unless (string-blank-p prompt)
    (pilish-agents--request "resume" (plist-get (pilish-agents--require-agent) :id) prompt)))

(defun pilish-agents--known-types ()
  "Return agent type names from the feed or agent definition files."
  (or (mapcar (lambda (ty) (plist-get ty :name)) (plist-get (pilish-agents--view) :types))
      (delete-dups
       (append '("general-purpose")
               (mapcan (lambda (dir)
                         (and (file-directory-p dir)
                              (mapcar #'file-name-base
                                      (directory-files dir nil "\\.md\\'"))))
                       (list (expand-file-name ".pi/agents"
                                               (pilish-agents--in-chat default-directory))
                             (expand-file-name "~/.pi/agent/agents")))))))

(defun pilish-agents-spawn (type prompt)
  "Spawn a background agent of TYPE with PROMPT."
  (interactive
   (list (completing-read "Agent type: " (pilish-agents--known-types) nil nil nil nil
                          "general-purpose")
         (read-string "Prompt: ")))
  (unless (string-blank-p prompt)
    (pilish-agents--request "spawn" type prompt)))

(defun pilish-agents--read-step (wf prompt states)
  "Read a step index of WF with PROMPT among steps in STATES."
  (let* ((choices (mapcar (lambda (a)
                            (cons (format "%d  %-22s %s" (plist-get a :index)
                                          (plist-get a :label) (plist-get a :state))
                                  (plist-get a :index)))
                          (seq-filter (lambda (a) (member (plist-get a :state) states))
                                      (plist-get wf :agents)))))
    (unless choices (user-error "No eligible workflow steps"))
    (cdr (assoc (completing-read prompt choices nil t) choices))))

(defun pilish-agents-workflow-toggle-pause ()
  "Pause the selected workflow, or resume it when paused."
  (interactive)
  (let ((wf (pilish-agents--require-workflow)))
    (pilish-agents--request "workflow"
                            (if (equal (plist-get wf :status) "paused") "resume" "pause")
                            (plist-get wf :id))))

(defun pilish-agents--pause-description ()
  "Return the pause suffix description for the selected workflow."
  (if (pilish-agents--workflow-status-p '("paused")) "resume" "pause"))

(defun pilish-agents-workflow-kill ()
  "Kill the selected workflow after confirmation."
  (interactive)
  (let ((wf (pilish-agents--require-workflow)))
    (when (y-or-n-p (format "Kill workflow %s? " (plist-get wf :name)))
      (pilish-agents--request "workflow" "kill" (plist-get wf :id)))))

(defun pilish-agents-workflow-skip ()
  "Skip a queued or running step of the selected workflow."
  (interactive)
  (let ((wf (pilish-agents--require-workflow)))
    (pilish-agents--request "workflow" "skip" (plist-get wf :id)
                            (number-to-string
                             (pilish-agents--read-step wf "Skip step: " '("queued" "running"))))))

(defun pilish-agents-workflow-retry ()
  "Retry a failed, skipped, or interrupted step of the selected workflow."
  (interactive)
  (let ((wf (pilish-agents--require-workflow)))
    (pilish-agents--request "workflow" "retry" (plist-get wf :id)
                            (number-to-string
                             (pilish-agents--read-step
                              wf "Retry step: " '("failed" "skipped" "interrupted" "done"))))))

(defun pilish-agents-select-next (&optional delta)
  "Select the next item, or move by DELTA items."
  (interactive)
  (pilish-agents--in-chat
    (let* ((items (pilish-agents--items (pilish-agents--view)))
           (pos (or (seq-position items pilish-agents--selected
                                  (lambda (it key) (equal (pilish-agents--item-key it) key)))
                    0)))
      (when items
        (setq pilish-agents--selected
              (pilish-agents--item-key
               (nth (mod (+ pos (or delta 1)) (length items)) items)))))))

(defun pilish-agents-select-previous ()
  "Select the previous item."
  (interactive)
  (pilish-agents-select-next -1))

(defun pilish-agents-cycle-filter ()
  "Cycle the agent list filter."
  (interactive)
  (pilish-agents--in-chat
    (setq pilish-agents--filter
          (pcase pilish-agents--filter ('recent 'active) ('active 'all) (_ 'recent))))
  (pilish-agents--refresh))

(defun pilish-agents--filter-description ()
  "Return the filter suffix description."
  (format "filter: %s" (pilish-agents--in-chat pilish-agents--filter)))

(defun pilish-agents-refresh ()
  "Refresh agent state now."
  (interactive)
  (clrhash pilish-agents--disk-cache)
  (when (eq (plist-get (pilish-agents--view) :source) 'feed)
    (pilish-agents--request "state"))
  (pilish-agents--refresh))

(defun pilish-agents-toggle-demo ()
  "Toggle the demo fleet for this session."
  (interactive)
  (pilish-agents--in-chat
    (setq pilish-agents--demo (if pilish-agents--demo nil (pilish-agents--make-demo))
          pilish-agents--selected nil))
  (pilish-agents--refresh))

(defun pilish-agents--demo-description ()
  "Return the demo toggle description."
  (if (pilish-agents--in-chat pilish-agents--demo) "demo: on" "demo: off"))

(defun pilish-agents-copy-id ()
  "Copy the selected item's id."
  (interactive)
  (let ((id (plist-get (cdr (or (pilish-agents--selected-item)
                                (user-error "Nothing selected")))
                       :id)))
    (kill-new id)
    (message "Copied %s" id)))

(defun pilish-agents-insert-reference ()
  "Insert a reference to the selected agent into the prompt draft."
  (interactive)
  (let* ((a (pilish-agents--require-agent))
         (ref (format "%s (agent %s)" (pilish-agents--label a)
                      (pilish-agents--short-id (plist-get a :id))))
         (input (pilish-agents--in-chat pilish--input-buffer)))
    (unless (buffer-live-p input) (user-error "No input buffer"))
    (with-current-buffer input (insert ref))
    (message "Inserted %s" ref)))

(defun pilish-agents--open-session (file session-id)
  "Open saved conversation FILE with SESSION-ID read-only."
  (require 'pilish-snapshot)
  (let* ((anchor (or (pilish-agents--in-chat (plist-get pilish--state :session-file)) file))
         (target (pilish--snapshot-target-for-pi-v3
                  anchor session-id (file-name-directory file)
                  (file-name-nondirectory file))))
    (unless target (user-error "Cannot open %s" file))
    (pilish-open-subagent-snapshot target)))

(defun pilish-agents-open-conversation ()
  "Open the selected agent's conversation read-only."
  (interactive)
  (if-let* ((wf (pilish-agents--selected-workflow)))
      (let* ((steps (seq-filter (lambda (a) (pilish-agents--get a :sessionFile))
                                (plist-get wf :agents)))
             (choices (mapcar (lambda (a) (cons (plist-get a :label) a)) steps))
             (step (cdr (assoc (completing-read "Conversation: " choices nil t) choices))))
        (pilish-agents--open-session (plist-get step :sessionFile) (plist-get step :sessionId)))
    (let ((a (pilish-agents--require-agent)))
      (unless (pilish-agents--get a :sessionFile)
        (user-error "[stub] %s has no saved conversation" (pilish-agents--label a)))
      (pilish-agents--open-session (plist-get a :sessionFile) (plist-get a :sessionId)))))

;;;; Feed example

(defun pilish-agents--json-value (value)
  "Return VALUE converted for `json-serialize'.
Plists become objects without nil or demo-internal keys; other lists
become arrays."
  (cond ((and (consp value) (keywordp (car value)))
         (let (out)
           (while value
             (let ((key (pop value)) (val (pop value)))
               (unless (or (null val) (eq key :demoFinishAt))
                 (setq out (append out (list key (pilish-agents--json-value val)))))))
           (or out (make-hash-table))))
        ((consp value) (vconcat (mapcar #'pilish-agents--json-value value)))
        (t value)))

(defun pilish-agents--feed-payload ()
  "Return the current view as a feed state plist."
  (let ((view (pilish-agents--view)))
    (list :version 1
          :generatedAt (pilish-agents--now-ms)
          :controls (or (plist-get view :controls) pilish-agents--all-controls)
          :types (or (plist-get view :types)
                     (mapcar (lambda (name) (list :name name)) (pilish-agents--known-types)))
          :agents (plist-get view :agents)
          :workflows (plist-get view :workflows))))

(defun pilish-agents-feed-example ()
  "Show the current view as a `subagents:state' feed payload.
This is the JSON pi-subagents should publish; see the Commentary."
  (interactive)
  (let ((json (json-serialize (pilish-agents--json-value (pilish-agents--feed-payload))))
        (buf (get-buffer-create "*pilish-agents feed example*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert json)
        (json-pretty-print-buffer)
        (when (fboundp 'js-json-mode) (js-json-mode))
        (goto-char (point-min))))
    (pop-to-buffer buf)))

(defun pilish-agents--inject-feed (chat state)
  "Deliver feed STATE plist to CHAT as pi would, through `setWidget'."
  (with-current-buffer chat
    (pilish--handle-extension-ui-request
     (list :type "extension_ui_request" :id "feed" :method "setWidget"
           :widgetKey pilish-agents-feed-widget-key
           :widgetLines (vector (json-serialize (pilish-agents--json-value state)))))))

;;;; Output buffer

(defvar-local pilish-agents--output-key nil
  "Selection key shown by this output buffer.")

(defvar-local pilish-agents--output-chat nil
  "Chat buffer owning this output buffer.")

(defvar pilish-agents-output-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'pilish-agents-output-refresh)
    (define-key map (kbd "o") #'pilish-agents-output-open-conversation)
    (define-key map (kbd "a") #'pilish-agents-output-menu)
    map)
  "Keymap for `pilish-agents-output-mode'.")

(define-derived-mode pilish-agents-output-mode special-mode "Pi-Agent"
  "Read-only view of one subagent's status and recent output."
  (setq-local truncate-lines nil)
  (setq-local word-wrap t))

(defun pilish-agents--output-render (agent)
  "Insert the full output view of AGENT."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (pilish-agents--agent-detail
             (plist-put (copy-sequence agent) :recent nil))
            "\n\n")
    (when-let* ((result (pilish-agents--get agent :resultPreview)))
      (insert (propertize "Result\n" 'face 'pilish-agents-heading) result "\n\n"))
    (insert (propertize "Recent output\n" 'face 'pilish-agents-heading))
    (dolist (r (pilish-agents--get agent :recent))
      (insert (pilish-agents--recent-line r) "\n"))
    (when-let* ((prompt (pilish-agents--get agent :prompt)))
      (insert "\n" (propertize "Prompt\n" 'face 'pilish-agents-heading) prompt "\n"))
    (insert "\n" (propertize "g refresh · o open conversation · a agents menu · q quit"
                             'face 'pilish-agents-meta))
    (goto-char (point-min))))

(defun pilish-agents-output-refresh ()
  "Refresh this output buffer."
  (interactive)
  (let ((key pilish-agents--output-key)
        (buf (current-buffer)))
    (let ((pilish-agents--chat-buffer pilish-agents--output-chat))
      (pilish-agents--refresh)
      (let ((agent (pilish-agents--in-chat
                     (seq-find (lambda (a) (equal (plist-get a :id) (cdr key)))
                               (plist-get pilish-agents--view :agents)))))
        (with-current-buffer buf
          (if agent (pilish-agents--output-render agent)
            (message "Agent no longer listed")))))))

(defun pilish-agents-output-open-conversation ()
  "Open the conversation of this output buffer's agent."
  (interactive)
  (let ((pilish-agents--chat-buffer pilish-agents--output-chat))
    (pilish-agents--in-chat (setq pilish-agents--selected pilish-agents--output-key))
    (pilish-agents-open-conversation)))

(defun pilish-agents-output-menu ()
  "Open the agents menu for this output buffer's session."
  (interactive)
  (let ((chat pilish-agents--output-chat))
    (with-current-buffer chat
      (setq pilish-agents--selected pilish-agents--output-key))
    (setq pilish-agents--chat-buffer chat)
    (pilish-agents-menu)))

(defun pilish-agents-show-output ()
  "Show the selected agent's full recent output in a side buffer."
  (interactive)
  (let* ((a (pilish-agents--require-agent))
         (chat (pilish-agents--chat))
         (buf (get-buffer-create (format "*pilish-agent:%s %s*" (pilish-agents--label a)
                                         (pilish-agents--short-id (plist-get a :id))))))
    (with-current-buffer buf
      (pilish-agents-output-mode)
      (setq pilish-agents--output-chat chat
            pilish-agents--output-key (cons 'agent (plist-get a :id)))
      (pilish-agents--output-render a))
    (display-buffer buf '((display-buffer-reuse-window display-buffer-pop-up-window)
                          (inhibit-same-window . t)))))

;;;; Live refresh

(defun pilish-agents--menu-active-p ()
  "Return non-nil when the agents transient is showing."
  (and (bound-and-true-p transient--prefix)
       (eq (oref transient--prefix command) 'pilish-agents-menu)))

(defun pilish-agents--tick ()
  "Advance demo state and redisplay the open menu."
  (if (not (pilish-agents--menu-active-p))
      (pilish-agents--stop-timer)
    (unless (active-minibuffer-window)
      (condition-case err
          (progn
            (cl-incf pilish-agents--tick)
            (when (buffer-live-p pilish-agents--chat-buffer)
              (with-current-buffer pilish-agents--chat-buffer
                (when pilish-agents--demo (pilish-agents--demo-tick pilish-agents--demo))))
            (pilish-agents--refresh)
            (let ((now (pilish-agents--suffix-signature))
                  (built (alist-get 'signature pilish-agents--rendered-keys)))
              ;; Rows redraw themselves.  A changed item set, selection or
              ;; selected status needs new suffixes, because which keys are
              ;; available is computed when the suffixes are built.
              (if (equal now built)
                  (transient--redisplay)
                (transient--refresh-transient))))
        (error (message "pilish-agents refresh: %s" (error-message-string err))
               (pilish-agents--stop-timer))))))

(defun pilish-agents--stop-timer ()
  "Stop the live refresh timer."
  (when (timerp pilish-agents--timer)
    (cancel-timer pilish-agents--timer))
  (setq pilish-agents--timer nil)
  (remove-hook 'transient-exit-hook #'pilish-agents--on-exit))

(defun pilish-agents--on-exit ()
  "Stop refreshing once the agents menu is gone."
  (run-at-time 0 nil (lambda ()
                       (unless (pilish-agents--menu-active-p)
                         (pilish-agents--stop-timer)))))

(defun pilish-agents--on-widget (key _lines)
  "Refresh the open menu when widget KEY is the state feed."
  (when (and (equal key pilish-agents-feed-widget-key)
             (eq (current-buffer) pilish-agents--chat-buffer))
    (setq pilish-agents--view nil)))

(add-hook 'pilish-extension-widget-functions #'pilish-agents--on-widget)
(add-to-list 'pilish-extension-data-widget-keys pilish-agents-feed-widget-key)

;;;; Transient

(transient-define-prefix pilish-agents-menu ()
  "Inspect and manage the current session's subagents and workflows."
  :refresh-suffixes t
  [:class transient-column (:info #'pilish-agents--header)]
  [:description pilish-agents--agents-heading
   :class transient-column
   :setup-children (lambda (_) (pilish-agents--children 'agent))]
  [:description (lambda () (propertize "Workflows" 'face 'pilish-agents-heading))
   :class transient-column
   :if pilish-agents--has-workflows-p
   :setup-children (lambda (_) (pilish-agents--children 'workflow))]
  [:class transient-column (:info #'pilish-agents--detail)]
  [["Agent"
    ("s" "steer…" pilish-agents-steer :transient t :inapt-if-not pilish-agents--can-steer-p)
    ("k" "interrupt" pilish-agents-interrupt :transient t
     :inapt-if-not pilish-agents--can-interrupt-p)
    ("x" "stop…" pilish-agents-stop :transient t :inapt-if-not pilish-agents--can-stop-p)
    ("R" "resume…" pilish-agents-resume :transient t :inapt-if-not pilish-agents--can-resume-p)
    ("N" "new agent…" pilish-agents-spawn :transient t :inapt-if-not pilish-agents--can-spawn-p)]
   ["Workflow"
    ("P" pilish-agents-workflow-toggle-pause :transient t
     :description pilish-agents--pause-description
     :inapt-if-not pilish-agents--can-pause-p)
    ("K" "kill…" pilish-agents-workflow-kill :transient t
     :inapt-if-not pilish-agents--can-kill-p)
    ("S" "skip step…" pilish-agents-workflow-skip :transient t
     :inapt-if-not pilish-agents--can-skip-p)
    ("Y" "retry step…" pilish-agents-workflow-retry :transient t
     :inapt-if-not pilish-agents--can-retry-p)]
   ["View"
    ("RET" "output" pilish-agents-show-output :inapt-if-not pilish-agents--selected-agent)
    ("o" "conversation" pilish-agents-open-conversation
     :inapt-if-not pilish-agents--has-session-p)
    ("y" "copy id" pilish-agents-copy-id :transient t)
    ("i" "insert ref" pilish-agents-insert-reference
     :inapt-if-not pilish-agents--selected-agent)]
   ["Navigate"
    ("n" "next" pilish-agents-select-next :transient t)
    ("p" "previous" pilish-agents-select-previous :transient t)
    ("f" pilish-agents-cycle-filter :transient t
     :description pilish-agents--filter-description)
    ("g" "refresh" pilish-agents-refresh :transient t)
    ("D" pilish-agents-toggle-demo :transient t
     :description pilish-agents--demo-description)
    ("q" "quit" transient-quit-one)]]
  (interactive)
  (setq pilish-agents--chat-buffer (or (pilish--get-chat-buffer)
                                       (and (buffer-live-p pilish-agents--chat-buffer)
                                            pilish-agents--chat-buffer)
                                       (user-error "No pi session in current buffer")))
  (pilish-agents--refresh)
  (transient-setup 'pilish-agents-menu)
  (pilish-agents--stop-timer)
  (add-hook 'transient-exit-hook #'pilish-agents--on-exit)
  (setq pilish-agents--timer
        (run-at-time pilish-agents-refresh-interval pilish-agents-refresh-interval
                     #'pilish-agents--tick)))

;;;; Entry points

(define-key pilish-chat-mode-map (kbd "C-c C-a") #'pilish-agents-menu)
(with-eval-after-load 'pilish-input
  (define-key pilish-input-mode-map (kbd "C-c C-a") #'pilish-agents-menu))

(setf (alist-get "subagents" pilish-extension-status-commands nil nil #'equal)
      #'pilish-agents-menu)

(provide 'pilish-agents)
;;; pilish-agents.el ends here
