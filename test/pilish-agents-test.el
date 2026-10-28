;;; pilish-agents-test.el --- Tests for pilish-agents -*- lexical-binding: t; -*-

;;; Commentary:

;; Source selection, feed parsing, child-session discovery, control
;; dispatch, demo controls, and row formatting for `pilish-agents-menu'.

;;; Code:

(require 'pilish-test-common)
(require 'pilish)
(require 'pilish-agents)

(defmacro pilish-agents-test--with-chat (&rest body)
  "Run BODY with a fresh chat buffer bound as the agents menu session."
  (declare (indent 0) (debug t))
  `(let ((chat (generate-new-buffer "*pilish-agents-test-chat*")))
     (unwind-protect
         (let ((pilish-agents--chat-buffer chat))
           (with-current-buffer chat (pilish-chat-mode))
           ,@body)
       (kill-buffer chat))))

(defun pilish-agents-test--feed (&rest agents)
  "Return a feed state plist with AGENTS and every control."
  (list :version 1 :controls (copy-sequence pilish-agents--all-controls)
        :agents agents :workflows nil))

(defun pilish-agents-test--agent (id status &rest props)
  "Return a feed agent ID with STATUS and extra PROPS."
  (append (list :id id :type "Explore" :description (concat "task " id)
                :status status :toolUses 3 :startedAt 1000)
          props))

(defun pilish-agents-test--write-jsonl (file records)
  "Write RECORDS as JSON lines to FILE."
  (with-temp-file file
    (dolist (r records)
      (insert (json-serialize r) "\n"))))

(ert-deftest pilish-agents-test-feed-widget-selects-feed-source ()
  "A `subagents:state' widget switches the view to the live feed."
  (pilish-agents-test--with-chat
    (should (eq (plist-get (pilish-agents--refresh) :source) 'disk))
    (pilish-agents--inject-feed
     chat (pilish-agents-test--feed (pilish-agents-test--agent "a1" "running")
                                    (pilish-agents-test--agent "w1" "running"
                                                               :workflowId "wf_1")))
    (let ((view (pilish-agents--refresh)))
      (should (eq (plist-get view :source) 'feed))
      ;; Workflow-owned agents are shown under their workflow only.
      (should (equal (mapcar (lambda (a) (plist-get a :id)) (plist-get view :agents))
                     '("a1"))))))

(ert-deftest pilish-agents-test-malformed-feed-falls-back ()
  "Malformed or versionless feed JSON is ignored."
  (should-not (pilish-agents--parse-feed '("{not json")))
  (should-not (pilish-agents--parse-feed '("{\"agents\":[]}")))
  (should (pilish-agents--parse-feed '("{\"version\":1,\"agents\":[]}"))))

(ert-deftest pilish-agents-test-demo-overrides-feed ()
  "Demo mode wins over a live feed until toggled off."
  (pilish-agents-test--with-chat
    (pilish-agents--inject-feed chat (pilish-agents-test--feed))
    (pilish-agents-toggle-demo)
    (should (eq (plist-get (pilish-agents--view) :source) 'demo))
    (pilish-agents-toggle-demo)
    (should (eq (plist-get (pilish-agents--view) :source) 'feed))))

(ert-deftest pilish-agents-test-disk-children-of-session ()
  "Child sessions naming this session as parent are listed with status."
  (let* ((dir (make-temp-file "pilish-agents-test" t))
         (parent (expand-file-name "2026-01-01T00-00-00-000Z_parent.jsonl" dir))
         (child (expand-file-name "2026-01-01T00-01-00-000Z_child.jsonl" dir))
         (other (expand-file-name "2026-01-01T00-02-00-000Z_other.jsonl" dir)))
    (unwind-protect
        (progn
          (pilish-agents-test--write-jsonl
           parent '((:type "session" :version 3 :id "parent" :timestamp "2026-01-01T00:00:00Z")))
          (pilish-agents-test--write-jsonl
           child
           `((:type "session" :version 3 :id "0123456789abcdef"
                    :timestamp "2026-01-01T00:01:00Z" :parentSession ,parent)
             (:type "session_info" :id "i1" :name "Explore#abcdef12")
             (:type "message" :id "m1" :timestamp "2026-01-01T00:01:01Z"
                    :message (:role "user" :content [(:type "text" :text "Find the bug\nmore")]))
             (:type "message" :id "m2" :timestamp "2026-01-01T00:01:05Z"
                    :message (:role "assistant" :stopReason "toolUse"
                                    :content [(:type "toolCall" :name "read"
                                                     :arguments (:path "a.el"))]
                                    :usage (:totalTokens 1200 :cost (:total 0.5))))
             (:type "message" :id "m3" :timestamp "2026-01-01T00:01:09Z"
                    :message (:role "assistant" :stopReason "stop"
                                    :content [(:type "text" :text "Fixed in a.el")]
                                    :usage (:totalTokens 1500 :cost (:total 0.25))))))
          (pilish-agents-test--write-jsonl
           other '((:type "session" :version 3 :id "other" :parentSession "/elsewhere.jsonl")))
          (clrhash pilish-agents--disk-cache)
          (clrhash pilish-agents--header-cache)
          (let ((agents (pilish-agents--disk-agents parent)))
            (should (= (length agents) 1))
            (let ((a (car agents)))
              (should (equal (plist-get a :id) "abcdef12"))
              (should (equal (plist-get a :type) "Explore"))
              (should (equal (plist-get a :description) "Find the bug"))
              (should (equal (plist-get a :status) "completed"))
              (should (= (plist-get a :toolUses) 1))
              (should (= (plist-get a :contextTokens) 1500))
              (should (= (plist-get a :cost) 0.75))
              (should (equal (plist-get a :resultPreview) "Fixed in a.el"))
              (should (equal (plist-get a :sessionFile) child))
              (should (equal (mapcar (lambda (r) (plist-get r :kind)) (plist-get a :recent))
                             '("tool" "text"))))))
      (delete-directory dir t))))

(ert-deftest pilish-agents-test-feed-controls-send-agents-command ()
  "Feed controls go to pi as bare /agents prompts, leaving session status alone."
  (pilish-agents-test--with-chat
    (pilish-agents--inject-feed chat (pilish-agents-test--feed
                                      (pilish-agents-test--agent "a1" "running")))
    (pilish-agents--refresh)
    (let ((proc (start-process "pilish-agents-test" nil "cat"))
          sent)
      (unwind-protect
          (progn
            (with-current-buffer chat
              (setq pilish--process proc
                    pilish--status 'streaming))
            (cl-letf (((symbol-function 'pilish--rpc-async)
                       (lambda (p command _callback)
                         (should (eq p proc))
                         (push command sent))))
              (pilish-agents-steer "focus on tests")
              (pilish-agents-interrupt)
              (pilish-agents--request "workflow" "kill" "wf_1"))
            (should (equal (reverse sent)
                           '((:type "prompt" :message "/agents steer a1 focus on tests")
                             (:type "prompt" :message "/agents interrupt a1")
                             (:type "prompt" :message "/agents workflow kill wf_1"))))
            ;; A control mid-turn must not touch the turn's status.
            (should (eq (buffer-local-value 'pilish--status chat) 'streaming)))
        (delete-process proc)))))

(ert-deftest pilish-agents-test-paused-workflow-clock-stops ()
  "A paused workflow's elapsed time ends at its pausedAt."
  (should (= (pilish-agents--elapsed '(:startedAt 1000 :pausedAt 5000 :pausedMs 500) :endedAt)
             3500))
  (should (= (pilish-agents--elapsed '(:startedAt 1000 :endedAt 9000 :pausedAt 5000) :endedAt)
             8000)))

(ert-deftest pilish-agents-test-feed-unadvertised-control-not-sent ()
  "A verb missing from the feed's controls is greyed and never sent."
  (pilish-agents-test--with-chat
    (pilish-agents--inject-feed
     chat (plist-put (pilish-agents-test--feed (pilish-agents-test--agent "a1" "running"))
                     :controls '("interrupt")))
    (pilish-agents--refresh)
    (should-not (pilish-agents--can-steer-p))
    (should (pilish-agents--can-interrupt-p))
    (let (sent)
      (cl-letf (((symbol-function 'pilish-run-command)
                 (lambda (&rest args) (push args sent))))
        (pilish-agents--request "steer" "a1" "hi"))
      (should-not sent))))

(ert-deftest pilish-agents-test-disk-controls-are-stubs ()
  "Without a feed, controls report the command and send nothing."
  (pilish-agents-test--with-chat
    (let (sent messages)
      (cl-letf (((symbol-function 'pilish-run-command)
                 (lambda (&rest args) (push args sent)))
                ((symbol-function 'message)
                 (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
        (pilish-agents--request "spawn" "Explore" "look around"))
      (should-not sent)
      (should (string-match-p "not sent.*/agents spawn Explore look around" (car messages))))))

(ert-deftest pilish-agents-test-demo-controls-mutate-fleet ()
  "Demo controls change the demo fleet like the real extension would."
  (pilish-agents-test--with-chat
    (pilish-agents-toggle-demo)
    (cl-letf (((symbol-function 'message) #'ignore))
      (let* ((agents (plist-get (pilish-agents--view) :agents))
             (running (seq-find (lambda (a) (equal (plist-get a :status) "running")) agents))
             (id (plist-get running :id))
             (wf (car (plist-get (pilish-agents--view) :workflows))))
        (with-current-buffer chat (setq pilish-agents--selected (cons 'agent id)))
        (pilish-agents-steer "narrow it down")
        (should (equal (plist-get (car (last (plist-get running :recent))) :text)
                       "narrow it down"))
        (pilish-agents-interrupt)
        (should (equal (plist-get running :status) "interrupted"))
        (should (pilish-agents--can-resume-p))
        (with-current-buffer chat
          (setq pilish-agents--selected (cons 'workflow (plist-get wf :id))))
        (pilish-agents-workflow-toggle-pause)
        (should (equal (plist-get wf :status) "paused"))
        (should (equal (pilish-agents--pause-description) "resume"))
        (pilish-agents-workflow-toggle-pause)
        (should (equal (plist-get wf :status) "running"))))))

(ert-deftest pilish-agents-test-selection-follows-key-and-wraps ()
  "Selection is by id, survives refresh, and n/p wrap around."
  (pilish-agents-test--with-chat
    (pilish-agents--inject-feed chat (pilish-agents-test--feed
                                      (pilish-agents-test--agent "a1" "running")
                                      (pilish-agents-test--agent "a2" "running")))
    (pilish-agents--refresh)
    (let ((first (with-current-buffer chat pilish-agents--selected)))
      (should (eq (car first) 'agent))
      (pilish-agents-select-next)
      (should-not (equal first (with-current-buffer chat pilish-agents--selected)))
      (pilish-agents-select-next)
      (should (equal first (with-current-buffer chat pilish-agents--selected)))
      (pilish-agents-select-previous)
      (pilish-agents--refresh)
      (should-not (equal first (with-current-buffer chat pilish-agents--selected))))))

(ert-deftest pilish-agents-test-recent-filter-limits-finished ()
  "The default filter keeps active agents and the newest finished ones."
  (pilish-agents-test--with-chat
    (let ((pilish-agents-recent-finished 2))
      (pilish-agents--inject-feed
       chat (apply #'pilish-agents-test--feed
                   (pilish-agents-test--agent "run" "running")
                   (mapcar (lambda (i)
                             (pilish-agents-test--agent (format "d%d" i) "completed"
                                                        :completedAt (* 1000 i)))
                           '(1 2 3))))
      (should (equal (mapcar (lambda (a) (plist-get a :id))
                             (pilish-agents--filtered-agents (pilish-agents--refresh)))
                     '("run" "d3" "d2")))
      (pilish-agents-cycle-filter)
      (should (equal (mapcar (lambda (a) (plist-get a :id))
                             (pilish-agents--filtered-agents (pilish-agents--view)))
                     '("run")))
      (pilish-agents-cycle-filter)
      (should (= 4 (length (pilish-agents--filtered-agents (pilish-agents--view))))))))

(ert-deftest pilish-agents-test-agent-row-columns ()
  "Rows show glyph, label, description, status, and counters."
  (let ((row (substring-no-properties
              (pilish-agents--agent-row
               (pilish-agents-test--agent "a1" "completed" :completedAt 66000 :tokens 12345)
               t))))
    (should (string-prefix-p "▸✓ Explore" row))
    (should (string-match-p "task a1 +completed +1m05s +3 tools +12k" row))))

(ert-deftest pilish-agents-test-workflow-steps-capped ()
  "Long workflows list unfinished steps and the newest finished ones."
  (let* ((steps (append (mapcar (lambda (i) (list :index i :label "s" :state "done"))
                                (number-sequence 0 9))
                        (list (list :index 10 :label "r" :state "running")
                              (list :index 11 :label "q" :state "queued"))))
         (shown (pilish-agents--workflow-visible-steps steps)))
    (should (= (length shown) pilish-agents--workflow-step-limit))
    (should (equal (mapcar (lambda (s) (plist-get s :index)) (last shown 2)) '(10 11)))
    (should (equal (plist-get (car shown) :index) 3))))

(ert-deftest pilish-agents-test-feed-payload-roundtrip ()
  "The example payload survives the setWidget path unchanged in shape."
  (pilish-agents-test--with-chat
    (pilish-agents-toggle-demo)
    (let ((payload (pilish-agents--feed-payload)))
      (pilish-agents-toggle-demo)
      (pilish-agents--inject-feed chat payload)
      (let ((view (pilish-agents--refresh)))
        (should (eq (plist-get view :source) 'feed))
        (should (= (length (plist-get view :agents)) (length (plist-get payload :agents))))
        (should (= (length (plist-get (car (plist-get view :workflows)) :agents))
                   (length (plist-get (car (plist-get payload :workflows)) :agents))))
        (should-not (plist-member (car (plist-get view :agents)) :demoFinishAt))))))

(provide 'pilish-agents-test)
;;; pilish-agents-test.el ends here
