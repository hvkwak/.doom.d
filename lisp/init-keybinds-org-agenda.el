;;; lisp/init-keybinds-org-agenda.el --- Keybindings for org-agenda -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary:
;;; org-agenda specific keybindings. Common keybindings are in init-keybinds-common.el
;;; Code:

(defun my/org-agenda-set-local-keys ()
  "IJKL movement in the agenda."
  (evil-local-set-key 'motion "i" #'org-agenda-previous-line)
  (evil-local-set-key 'motion "k" #'org-agenda-next-line)
  (evil-local-set-key 'motion "j" #'org-agenda-earlier)
  (evil-local-set-key 'motion "l" #'org-agenda-later)
  (evil-normalize-keymaps))

(add-hook 'org-agenda-mode-hook #'my-enable-common-keys)
(add-hook 'org-agenda-mode-hook #'my/org-agenda-set-local-keys 90)

;; ORG SCHEDULE / READ DATE CALENDAR KEYBINDINGS
;; evil-collection의 덮어쓰기를 무력화하기 위해 미니버퍼 생성 직후 버퍼-로컬 레벨에서 강제 설정
(defun my/setup-org-read-date-keybinds ()
  "Org calendar minibuffer가 열릴 때 M-ijkl 키를 강제로 고정."
  (when (bound-and-true-p org-read-date-minibuffer-local-map)
    (define-key org-read-date-minibuffer-local-map (kbd "M-i") #'org-calendar-backward-week)
    (define-key org-read-date-minibuffer-local-map (kbd "M-k") #'org-calendar-forward-week)
    (define-key org-read-date-minibuffer-local-map (kbd "M-j") #'org-calendar-backward-day)
    (define-key org-read-date-minibuffer-local-map (kbd "M-l") #'org-calendar-forward-day)
    (define-key org-read-date-minibuffer-local-map (kbd "M-h") nil)))

;; minibuffer setup 훅의 맨 마지막(depth 99)에 등록하여 evil-collection보다 늦게 실행되도록 보장
(add-hook 'minibuffer-setup-hook #'my/setup-org-read-date-keybinds 99)

(provide 'init-keybinds-org-agenda)
;;; init-keybinds-org-agenda.el ends here
