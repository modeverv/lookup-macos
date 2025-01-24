;; nmacos.el -- MacOS Dictionary.app lookup agent -*- lexical-binding: t -*-

(require 'lookup)

(defgroup nmacos nil
  "Lookup agent for MacOS Dictionary.app."
  :group 'lookup-agents)

(defvar nmacos-previous-window-configuration nil
  "Previous window configuration before dictionary display.")

(put 'nmacos :methods '(exact prefix suffix keyword substring))
(put 'nmacos 'content 'nmacos-dictionary-content)
(defun nmacos-dictionary-content (dictionary entry)
  "Get content for ENTRY in DICTIONARY."
  (lookup-entry-code entry))

(put 'nmacos 'setup 'nmacos-setup)
(defun nmacos-setup (agent)
  "Setup AGENT for MacOS dictionary."
  (let ((dict-list (nmacos-list agent)))
    (lookup-agent-set-title agent "MacOS Dictionary")
    (lookup-agent-set-dictionaries agent dict-list)
    dict-list))

(put 'nmacos 'list 'nmacos-list)
(defun nmacos-list (agent)
  "Return list of dictionaries available for AGENT."
  (let ((dictionaries '("English" "Japanese" "Japanese - English")))
    (mapcar (lambda (name)
              (lookup-new-dictionary agent name name name))
            dictionaries)))

(put 'nmacos 'search 'nmacos-dictionary-search)
(defun nmacos-dictionary-search (dictionary query)
  "Search DICTIONARY for QUERY using osx-dictionary command."
  (let* ((method (lookup-query-method query))
         (word (lookup-query-string query))
         (dict-name (lookup-dictionary-name dictionary)))
    (let* ((cmd (format "osx-dictionary -d %s %s"
                        (shell-quote-argument dict-name)
                        (shell-quote-argument word)))
           (content (shell-command-to-string cmd))
           entry-content entries)
      (when (string-match "from: [^\n]+\n+\\(\\(.\\|\n\\)*\\)\\'" content)
        (setq entry-content (match-string 1 content))
        (unless (string-empty-p (string-trim entry-content))
          (let ((entry (lookup-make-entry dictionary entry-content word)))
            (lookup-arrange-heading entry)
            (setq entries (list entry))
            entries))))))

(put 'nmacos 'content 'nmacos-dictionary-content)
(defun nmacos-dictionary-content (dictionary entry)
  "Get and format content for ENTRY in DICTIONARY."
  (let* ((word (lookup-entry-heading entry))
         (dict-name (lookup-dictionary-name dictionary))
         (cmd (format "osx-dictionary -d %s %s"
                      (shell-quote-argument dict-name)
                      (shell-quote-argument word)))
         (raw-content (shell-command-to-string cmd)))
    (if (string-match "from: [^\n]+\n+\\(\\(.\\|\n\\)*\\)\\'" raw-content)
        (let ((formatted-content (match-string 1 raw-content)))
          (with-temp-buffer
            (insert formatted-content)
            (goto-char (point-min))

            ;; Remove extra whitespace
            (while (re-search-forward "^[ \t\r\n]+" nil t)
              (replace-match ""))

            ;; Add line breaks for better readability
            (goto-char (point-min))
            (while (re-search-forward "\\(|[^|]+|\\)" nil t)
              (replace-match "\\1\n"))
            (goto-char (point-min))
            (while (re-search-forward "\\([^|\n]\\)\\(形容詞\\|名詞\\|動詞\\)" nil t)
              (replace-match "\\1\n\\2"))
            (goto-char (point-min))
            (while (re-search-forward "[^▸\n]\\(▸\\)" nil t)
              (replace-match "\n\\1"))
            (goto-char (point-min))
            (while (re-search-forward "\\(【[^】]+】\\)" nil t)
              (replace-match "\n\\1"))

            ;; Add blank lines for major sections
            (goto-char (point-min))
            (while (re-search-forward "\\([A-Z]+\\) " nil t)
              (when (member (match-string 1) '("DERIVATIVES" "ORIGIN"))
                (replace-match "\n\n\\1 ")))

            (buffer-string)))
      "")))

(provide 'nmacos)
