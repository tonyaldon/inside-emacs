# About
`inside-emacs` is a top-up for the video series **inside emacs**.

Watch [here](https://youtu.be/F1IXixEhQwk).

# Content

If you want to reproduce some of the actions made in **inside
emacs** videos, here you'll find:

1. Various list of the commands I use in the videos and the packages
   they belong to,
2. The code of the customized commands I use in the videos (I put them
   in `utils.el` file in the corresponding subdirectories of the
   [src](./src/) directory), they generaly are prefixed by `ta-`
   (stands for Tony Aldon),
3. When it's possible, the files I use to make the videos (see the
   [src](./src/) directory).

# Emacs "keybinding" paradigm

I've rebound mostly every standard keybindings. I've not thought about
the general use of these bindings, I just wanted them to fit the
specific [keyboard-layout](https://github.com/tonyaldon/keyboard-layout)
I'm using.

[hydra](https://github.com/abo-abo/hydra) package offers a flexible
and convenient way to make a bunch of related commands share a same
key sequence prefix.

I set `hydra-hint-display-type` to `'message` to not have the
minibuffer changed when I use an `hydra`.  And, all my `hydras` have
the parameter `:hint` set to `nil`.  I never want to see the keys
printed.

If I can't remember a key binding, mostly it's because I almost don't
use the command it maps to.  In that case, I remove it.

I also use [key-chord](https://github.com/emacsorphanage/key-chord)
package.  From the its documentation: This package implements support
for mapping a pair of simultaneously pressed keys to a command and for
mapping the same key being pressed twice in quick succession to a
command. Such bindings are called "key chords".

For more details, you can browse my
[emacs configuration](https://github.com/tonyaldon/emacs.d).

# Motivation

1. I want to take some live notes on my `emacs` workflow.
2. I want to own the best `emacs` commands, I want them to be part of me
   and want to be able to play with them as a musician would do.
3. I love `emacs` but I doesn't want to think about it when I'm using
   it. Therefore I take time out of my schedule to learn it. These
   videos are part of this learning process.

My main goal doing these videos is to be **quicker**, **clearer** and
more **fluent** when I **read**, **modify** or **write** code.

I hope these videos can help you in your `emacs` trip.

# Inside Emacs 1

[Watch on youtube](https://youtu.be/F1IXixEhQwk)

<details>
  <summary>Commands</summary>

| commands                         | packages                                                           |
|----------------------------------|--------------------------------------------------------------------|
| `avy-goto-word-or-subword-1`     | [avy](https://github.com/abo-abo/avy)                              |
| `cleanup-buffer`                 | [customized](./src/inside-emacs-1/utils.el)                        |
| `company-complete-selection`     | [company](https://github.com/company-mode/company-mode)            |
| `er/expand-region`               | [expand-region](https://github.com/magnars/expand-region.el)       |
| `iedit-mode`                     | [iedit](https://github.com/victorhge/iedit)                        |
| `iy-go-to-char`                  | [iy-go-to-char](https://github.com/doitian/iy-go-to-char)          |
| `iy-go-to-char-kill-region`      | [iy-go-to-char](https://github.com/doitian/iy-go-to-char)          |
| `iy-go-to-char-kill-ring-save`   | [iy-go-to-char](https://github.com/doitian/iy-go-to-char)          |
| `mc--insert-number-and-increase` | [multiple-cursors](https://github.com/magnars/multiple-cursors.el) |
| `mc/mark-next-like-this`         | [multiple-cursors](https://github.com/magnars/multiple-cursors.el) |
| `mc/mark-next-like-this-word`    | [multiple-cursors](https://github.com/magnars/multiple-cursors.el) |
| `replace-string`                 | built-in                                                           |
| `scroll-left`                    | built-in                                                           |
| `scroll-right`                   | built-in                                                           |
| `ta-avy-goto-end-of-line`        | [customized](./src/inside-emacs-1/utils.el)                        |
| `ta-mark-sexp-at-point`          | [customized](./src/inside-emacs-1/utils.el)                        |
| `ta-toggle-narrow`               | [customized](./src/inside-emacs-1/utils.el)                        |
| `ta-yank-line-below`             | [customized](./src/inside-emacs-1/utils.el)                        |
| `yank-rectangle`                 | built-in                                                           |

</details>

Video grabbed with my [emacs configuration](https://github.com/tonyaldon/emacs.d)
at commit `f760601bfc92bac7570f74396dbe1e4910af86af`.

# Inside Emacs 2

[Watch on youtube](https://youtu.be/Tc4-sLf8HBc)

<details>
  <summary>Commands</summary>

| commands                         | packages                                                           |
|----------------------------------|--------------------------------------------------------------------|
| `avy-goto-word-or-subword-1`     | [avy](https://github.com/abo-abo/avy)                              |
| `dired-do-find-marked-files`     | built-in                                                           |
| `dired-mark`                     | built-in                                                           |
| `dired-narrow`                   | [dired-narrow](https://melpa.org/#/dired-narrow)                   |
| `dired-unmark-all-marks`         | built-in                                                           |
| `er/expand-region`               | [expand-region](https://github.com/magnars/expand-region.el)       |
| `forward-paragraph`              | built-in                                                           |
| `isearch-backward`               | built-in                                                           |
| `isearch-forward`                | built-in                                                           |
| `kmacro-bind-to-key`             | built-in                                                           |
| `kmacro-end-macro`               | built-in                                                           |
| `kmacro-start-macro`             | built-in                                                           |
| `mc--insert-number-and-increase` | [multiple-cursors](https://github.com/magnars/multiple-cursors.el) |
| `mc/mark-next-like-this`         | [multiple-cursors](https://github.com/magnars/multiple-cursors.el) |
| `replace-string`                 | built-in                                                           |
| `set-mark-command`               | built-in                                                           |
| `ta-mark-inside-quotes-or-pairs` | [customized](./src/inside-emacs-2/utils.el)                        |
| `ta-toggle-write-mode`           | [customized](./src/inside-emacs-2/utils.el)                        |
| `ta-w-finish-edit`               | [customized](./src/inside-emacs-2/utils.el)                        |
| `transpose-frame`                | [transpose-frame](https://melpa.org/#/transpose-frame)             |
| `universal-argument`             | built-in                                                           |
| `winner-undo`                    | built-in                                                           |
| `yas-expand`                     | [yasnippet](https://github.com/joaotavora/yasnippet)               |

</details>

Video grabbed with my [emacs configuration](https://github.com/tonyaldon/emacs.d)
at commit `f760601bfc92bac7570f74396dbe1e4910af86af`.

# Inside Emacs 3

[Watch on youtube](https://youtu.be/RFhay0n7JJo)

<details>
  <summary>Commands</summary>

| commands                           | packages                                                           |
|------------------------------------|--------------------------------------------------------------------|
| `mc/add-cursor-on-click`           | [multiple-cursors](https://github.com/magnars/multiple-cursors.el) |
| `sp-splice-sexp`                   | [smartparens](https://github.com/Fuco1/smartparens)                |
| `sp-backward-kill-sexp`            | [smartparens](https://github.com/Fuco1/smartparens)                |
| `ta-mark-sexp-at-point`            | [customized](./src/inside-emacs-3/utils.el)                        |
| `yas-expand`                       | [yasnippet](https://github.com/joaotavora/yasnippet)               |
| `drag-stuff-down`                  | [drag-stuff](https://github.com/rejeep/drag-stuff.el)              |
| `ta-aw-other-window-scroll-buffer` | [customized](./src/inside-emacs-3/utils.el)                        |
| `scroll-other-window`              | built-in                                                           |
| `scroll-other-window-down`         | built-in                                                           |
| `mc/mark-next-like-this`           | [multiple-cursors](https://github.com/magnars/multiple-cursors.el) |

</details>

Video grabbed with my [emacs configuration](https://github.com/tonyaldon/emacs.d)
at commit `4cbdb3ce735ae296387069ef7ff309f0cfa812e8`.

# Inside Emacs 4

The github repository of [company](https://github.com/company-mode/company-mode).

## Inside Emacs 4 (part 1)

[Watch on youtube](https://youtu.be/96jn5A73-oQ)

<details>
  <summary>Commands</summary>

| commands                     | packages                                                |
|------------------------------|---------------------------------------------------------|
| `avy-goto-word-or-subword-1` | [avy](https://github.com/abo-abo/avy)                   |
| `bicycle-cycle`              | [bicycle](https://github.com/tarsius/bicycle)           |
| `company-complete-selection` | [company](https://github.com/company-mode/company-mode) |
| `company-filter-candidates`  | [company](https://github.com/company-mode/company-mode) |
| `counsel-outline`            | [counsel](https://github.com/abo-abo/swiper)            |
| `delete-blank-lines`         | built-in                                                |
| `eval-defun`                 | built-in                                                |
| `join-line`                  | built-in                                                |
| `open-line`                  | built-in                                                |
| `ta-avy-copy-sexp`           | [customized](./src/inside-emacs-4/utils.el)             |
| `ta-mark-sexp-at-point`      | [customized](./src/inside-emacs-4/utils.el)             |
| `ta-outline-toggle-global`   | [customized](./src/inside-emacs-4/utils.el)             |
| `ta-sidebar`                 | [customized](./src/inside-emacs-4/utils.el)             |
| `yas-expand`                 | [yasnippet](https://github.com/joaotavora/yasnippet)    |

</details>

Video grabbed with my [emacs configuration](https://github.com/tonyaldon/emacs.d)
at commit `4cbdb3ce735ae296387069ef7ff309f0cfa812e8`.

## Inside Emacs 4 (part 2)

[Watch on youtube](https://youtu.be/zSPraaX2524)

<details>
  <summary>Commands</summary>

| commands                     | packages                                             |
|------------------------------|------------------------------------------------------|
| `avy-goto-word-or-subword-1` | [avy](https://github.com/abo-abo/avy)                |
| `counsel-M-x`                | [counsel](https://github.com/abo-abo/swiper)         |
| `delete-blank-lines`         | built-in                                             |
| `describe-variable`          | built-in                                             |
| `eval-defun`                 | built-in                                             |
| `eval-region`                | built-in                                             |
| `mark-paragraph`             | built-in                                             |
| `next-buffer`                | built-in                                             |
| `previous-buffer`            | built-in                                             |
| `revert-buffer`              | built-in                                             |
| `sp-next-sexp`               | [smartparens](https://github.com/Fuco1/smartparens)  |
| `swiper`                     | [swiper](https://github.com/abo-abo/swiper)          |
| `ta-above-new-indent`        | [customized](./src/inside-emacs-4/utils.el)          |
| `ta-avy-goto-end-of-line`    | [customized](./src/inside-emacs-4/utils.el)          |
| `ta-below-new-indent`        | [customized](./src/inside-emacs-4/utils.el)          |
| `ta-mark-sexp-at-point`      | [customized](./src/inside-emacs-4/utils.el)          |
| `window-left`                | built-in                                             |
| `window-right`               | built-in                                             |
| `window-toggle-side-windows` | built-in                                             |
| `yas-expand`                 | [yasnippet](https://github.com/joaotavora/yasnippet) |

</details>

Video grabbed with my [emacs configuration](https://github.com/tonyaldon/emacs.d)
at commit `4cbdb3ce735ae296387069ef7ff309f0cfa812e8`.

# Inside Emacs 5

## Inside Emacs 5 (part 1)

[Watch on youtube](https://youtu.be/kw4h2hYYq-o)

<details>
<summary>Command lines</summary>

	emacsclient -h
	emacsclient -nw
	emacs --daemon
	ps -aux | grep emacs
	emacsclient --eval "(kill-emacs)"
	man pidof
	pidof emacs
	ps -ax | grep emacs
	emacs -nw

</details>

Video grabbed with my [emacs configuration](https://github.com/tonyaldon/emacs.d)
at commit `33c04510f94e5eb96ed7b072dfba410cbc70a9d0`.

## Inside Emacs 5 (part 2)

[Watch on youtube](https://youtu.be/fg_jTo9SK9I)

<details>
<summary>Command lines</summary>

	cd ~/.config/systemd/user
	locate emacs.service
	cp /usr/share/emacs/28.0.50/etc/emacs.service .
	emacsclient -nw
	emacsclient --eval "(kill-emacs)"
	ps -ax | grep emacs
	systemctl enable emacs --user
	systemctl status emacs --user
	systemctl start emacs --user
	emacsclient -nc

</details>

Video grabbed with my [emacs configuration](https://github.com/tonyaldon/emacs.d)
at commit `33c04510f94e5eb96ed7b072dfba410cbc70a9d0`.

# Inside Emacs 6

## Inside Emacs 6 (part 1)

[Watch on youtube](https://youtu.be/pRXRwQ1GGr4)

Some modes used in the video: `org-mode`, `whitespace-mode`,
`tsv-mode` and `csv-mode`.

<details>
  <summary>Commands</summary>

| commands                                  | default key bindings | packages                                                           |
|-------------------------------------------|----------------------|--------------------------------------------------------------------|
| `avy-goto-line`                           |                      | [avy](https://github.com/abo-abo/avy)                              |
| `csv-align-fields`                        | C-c C-a              | [csv-mode](https://elpa.gnu.org/packages/csv-mode.html)            |
| `er/expand-region`                        |                      | [expand-region](https://github.com/magnars/expand-region.el)       |
| `mc/mark-next-like-this-word`             |                      | [multiple-cursors](https://github.com/magnars/multiple-cursors.el) |
| `org-ctrl-c-ctrl-c`                       | C-c C-c              | built-in                                                           |
| `org-cycle`                               | TAB                  | built-in                                                           |
| `org-return`                              | RET                  | built-in                                                           |
| `org-table-create-or-convert-from-region` | C-&#124;             | built-in                                                           |
| `previous-buffer`                         | C-x &lt;left&gt;     | built-in                                                           |
| `tsv-mode`                                |                      | [csv-mode](https://elpa.gnu.org/packages/csv-mode.html)            |
| `universal-argument`                      | C-u                  | built-in                                                           |
| `whitespace-mode`                         |                      | built-in                                                           |

</details>

Video grabbed with my [emacs configuration](https://github.com/tonyaldon/emacs.d)
at commit `fc02ec1c2d2caa52b22410adb465f79c115c7e2a`.

## Inside Emacs 6 (part 2)

[Watch on youtube](https://youtu.be/RhwCClMdaps)

<details>
  <summary>Emacs Lisp</summary>

| functions/variables                | docstring                                                                                   |
|------------------------------------|---------------------------------------------------------------------------------------------|
| `command-remapping`                | Return the remapping for command COMMAND.                                                   |
| `org-at-table-hline-p`             | Non-nil when point is inside a hline in a table.                                            |
| `org-at-table-p`                   | Non-nil if the cursor is inside an Org table.                                               |
| `org-table-current-column`         | Return current column number.                                                               |
| `org-table-current-line`           | Return the index of the current data line.                                                  |
| `org-table-goto-column`            | Move the cursor to the Nth column in the current table line.                                |
| `org-table-maybe-eval-formula`     | Check if the current field starts with "=" or ":=". If yes, store the formula and apply it. |
| `org-table-maybe-recalculate-line` | Recompute the current line if marked for it, and if we haven’t just done it.                |

</details>

<details>
  <summary>Commands</summary>

| commands                      | default key bindings | packages                                                           |
|-------------------------------|----------------------|--------------------------------------------------------------------|
| `avy-goto-char`               |                      | [avy](https://github.com/abo-abo/avy)                              |
| `avy-goto-line`               |                      | [avy](https://github.com/abo-abo/avy)                              |
| `er/expand-region`            |                      | [expand-region](https://github.com/magnars/expand-region.el)       |
| `eval-defun`                  |                      | built-in                                                           |
| `eval-expression`             | M-:                  | built-in                                                           |
| `iy-go-to-char`               |                      | [iy-go-to-char](https://github.com/doitian/iy-go-to-char)          |
| `join-line`                   |                      | built-in                                                           |
| `kill-ring-save`              | M-w                  | built-in                                                           |
| `mc/mark-next-like-this-word` |                      | [multiple-cursors](https://github.com/magnars/multiple-cursors.el) |
| `my-org-table-previous-row`   |                      | [customized](./src/inside-emacs-6/part-2/previous-row.el)          |
| `next-buffer`                 | C-x &lt;right&gt;    | built-in                                                           |
| `org-backward-sentence`       | M-a                  | built-in                                                           |
| `org-cycle`                   | TAB                  | built-in                                                           |
| `org-forward-sentence`        | M-e                  | built-in                                                           |
| `org-return`                  | RET                  | built-in                                                           |
| `org-shifttab`                | S-TAB                | built-in                                                           |
| `previous-buffer`             | C-x &lt;left&gt;     | built-in                                                           |
| `sp-backward-sexp`            |                      | [smartparens](https://github.com/Fuco1/smartparens)                |
| `sp-backward-up-sexp`         |                      | [smartparens](https://github.com/Fuco1/smartparens)                |
| `sp-forward-slurp-sexp`       |                      | [smartparens](https://github.com/Fuco1/smartparens)                |
| `sp-kill-sexp`                |                      | [smartparens](https://github.com/Fuco1/smartparens)                |
| `sp-next-sexp`                |                      | [smartparens](https://github.com/Fuco1/smartparens)                |
| `sp-raise-sexp`               |                      | [smartparens](https://github.com/Fuco1/smartparens)                |
| `ta-avy-copy-sexp`            |                      | [customized](./src/inside-emacs-6/part-2/utils.el)                 |
| `ta-avy-goto-end-of-line`     |                      | [customized](./src/inside-emacs-6/part-2/utils.el)                 |
| `ta-below-new-indent`         |                      | [customized](./src/inside-emacs-6/part-2/utils.el)                 |
| `ta-kill-whole-line`          |                      | [customized](./src/inside-emacs-6/part-2/utils.el)                 |
| `ta-mark-inside-dwim`         |                      | [customized](./src/inside-emacs-6/part-2/utils.el)                 |
| `ta-mark-sexp-at-point`       |                      | [customized](./src/inside-emacs-6/part-2/utils.el)                 |

</details>

Video grabbed with my [emacs configuration](https://github.com/tonyaldon/emacs.d)
at commit `6982f26e031a6aaa82515274d8572204fffaec56`.

## Inside Emacs 6 (part 3)

[Watch on youtube](https://youtu.be/KxOwKK5sXRA)

<details>
  <summary>Emacs Lisp</summary>

| functions/variables  | docstring                                                                 |
|----------------------|---------------------------------------------------------------------------|
| `call-interactively` | Call FUNCTION, providing args according to its interactive calling specs. |
| `define-key`         | In KEYMAP, define key sequence KEY as DEF.                                |
| `org-at-table-p`     | Non-nil if the cursor is inside an Org table.                             |
| `org-in-item-p`      | Return item beginning position when in a plain list, nil otherwise.       |
| `org-insert-heading` | Insert a new heading or an item with the same depth at point.             |
| `org-insert-item`    | Insert a new item at the current level.                                   |

</details>

<details>
  <summary>Commands</summary>

| commands                    | default key bindings | packages                                                           |
|-----------------------------|----------------------|--------------------------------------------------------------------|
| `avy-goto-char`             |                      | [avy](https://github.com/abo-abo/avy)                              |
| `avy-goto-line`             |                      | [avy](https://github.com/abo-abo/avy)                              |
| `eval-defun`                |                      | built-in                                                           |
| `join-line`                 |                      | built-in                                                           |
| `mc/mark-next-like-this`    |                      | [multiple-cursors](https://github.com/magnars/multiple-cursors.el) |
| `my-org-meta-return`        |                      | [customized](./src/inside-emacs-6/part-3/org-meta-return.el)       |
| `org-ctrl-c-ctrl-c`         | C-c C-c              | built-in                                                           |
| `org-meta-return`           | &lt;M-return&gt;     | built-in                                                           |
| `org-return`                | RET                  | built-in                                                           |
| `org-table-wrap-region`     |                      | built-in                                                           |
| `previous-buffer`           | C-x &lt;left&gt;     | built-in                                                           |
| `repeat`                    | C-x z                | built-in                                                           |
| `sp-backward-up-sexp`       |                      | [smartparens](https://github.com/Fuco1/smartparens)                |
| `sp-forward-slurp-sexp`     |                      | [smartparens](https://github.com/Fuco1/smartparens)                |
| `ta-avy-copy-sexp`          |                      | [customized](./src/inside-emacs-6/part-3/utils.el)                 |
| `ta-avy-goto-end-of-line`   |                      | [customized](./src/inside-emacs-6/part-3/utils.el)                 |
| `ta-kill-whole-line`        |                      | [customized](./src/inside-emacs-6/part-3/utils.el)                 |
| `ta-mark-inside-dwim`       |                      | [customized](./src/inside-emacs-6/part-3/utils.el)                 |
| `ta-mark-sexp-at-point`     |                      | [customized](./src/inside-emacs-6/part-3/utils.el)                 |
| `ta-org-table-previous-row` |                      | [customized](./src/inside-emacs-6/part-3/utils.el)                 |
| `undo`                      | C-x u                | built-in                                                           |
| `yas-expand`                |                      | [yasnippet](https://github.com/joaotavora/yasnippet)               |

</details>

Video grabbed with my [emacs configuration](https://github.com/tonyaldon/emacs.d)
at commit `124a1958e4e222722980ced00724f0ee7c948575`.

## Inside Emacs 6 (part 4)

[Watch on youtube](https://youtu.be/0fbrVArRxUo)

<details>
  <summary>Commands</summary>

| commands                    | default key bindings | packages                              |
|-----------------------------|----------------------|---------------------------------------|
| `avy-goto-char`             |                      | [avy](https://github.com/abo-abo/avy) |
| `org-ctrl-c-ctrl-c`         | C-c C-c              | built-in                              |
| `org-ctrl-c-minus`          | C-c -                | built-in                              |
| `org-ctrl-c-ret`            | C-c RET              | built-in                              |
| `org-cycle`                 | TAB                  | built-in                              |
| `org-forward-sentence`      | M-e                  | built-in                              |
| `org-return`                | RET                  | built-in                              |
| `org-shiftmetaright`        | M-S-&lt;right&gt;    | built-in                              |
| `org-shifttab`              | S-TAB                | built-in                              |
| `org-sort`                  | C-c ^                | built-in                              |
| `org-table-copy-region`     |                      | built-in                              |
| `org-table-hline-and-move`  | C-c RET              | built-in                              |
| `org-table-insert-hline`    | C-c -                | built-in                              |
| `org-table-paste-rectangle` |                      | built-in                              |
| `org-table-sort-lines`      | C-c ^                | built-in                              |
| `undo`                      | C-x u                | built-in                                                           |

</details>

Video grabbed with my [emacs configuration](https://github.com/tonyaldon/emacs.d)
at commit `124a1958e4e222722980ced00724f0ee7c948575`.

## Inside Emacs 6 (part 5)

[Watch on youtube](https://youtu.be/w4wxGOijyZs)

<details>
  <summary>Emacs Lisp</summary>

| functions/variables          | docstring                                                                      |
|------------------------------|--------------------------------------------------------------------------------|
| `org-table-auto-blank-field` | Non-nil means automatically blank table field when starting to type into it.   |
| `org-table-copy-increment`   | Non-nil means increment when copying current field with `org-table-copy-down`. |

</details>

<details>
  <summary>Commands</summary>

| commands                    | default key bindings | packages                                           |
|-----------------------------|----------------------|----------------------------------------------------|
| `avy-goto-char`             |                      | [avy](https://github.com/abo-abo/avy)              |
| `eval-expression`           | M-:                  | built-in                                           |
| `org-cycle`                 | TAB                  | built-in                                           |
| `org-metadown`              | M-<down>             | built-in                                           |
| `org-metaleft`              | M-<left>             | built-in                                           |
| `org-metaright`             | M-<right>            | built-in                                           |
| `org-metaup`                | M-<up>               | built-in                                           |
| `org-return`                | RET                  | built-in                                           |
| `org-shiftdown`             | S-<down>             | built-in                                           |
| `org-shiftdown`             | S-<down>             | built-in                                           |
| `org-shiftleft`             | S-<left>             | built-in                                           |
| `org-shiftleft`             | S-<left>             | built-in                                           |
| `org-shiftmetadown`         | M-S-<down>           | built-in                                           |
| `org-shiftmetaleft`         | M-S-<left>           | built-in                                           |
| `org-shiftmetaright`        | M-S-<right>          | built-in                                           |
| `org-shiftmetaup`           | M-S-<up>             | built-in                                           |
| `org-shiftright`            | S-<right>            | built-in                                           |
| `org-shiftright`            | S-<right>            | built-in                                           |
| `org-shifttab`              | S-TAB                | built-in                                           |
| `org-shiftup`               | S-<up>               | built-in                                           |
| `org-shiftup`               | S-<up>               | built-in                                           |
| `org-table-blank-field`     | C-SPC                | built-in                                           |
| `org-table-copy-down`       | S-RET                | built-in                                           |
| `ta-mark-inside-dwim`       |                      | [customized](./src/inside-emacs-6/part-5/utils.el) |
| `ta-mark-sexp-at-point`     |                      | [customized](./src/inside-emacs-6/part-5/utils.el) |
| `ta-org-table-previous-row` |                      | [customized](./src/inside-emacs-6/part-5/utils.el) |

</details>

Video grabbed with my [emacs configuration](https://github.com/tonyaldon/emacs.d)
at commit `852afb87e258c90a8e79a026dae369272b3b5280`.

## Inside Emacs 6 (part 6)

[Watch on youtube](https://www.youtube.com/watch?v=loa1g9wwNlI)

<details>
  <summary>Emacs Lisp</summary>

| functions/variables | docstring                                                |
|---------------------|----------------------------------------------------------|
| `orgtbl-to-generic` | Convert the orgtbl-mode TABLE to some other format.      |
| `orgtbl-to-tsv`     | Convert the orgtbl-mode table to TAB separated material. |

</details>

<details>
  <summary>Commands</summary>

| commands                        | default key bindings | packages                                           |
|---------------------------------|----------------------|----------------------------------------------------|
| `avy-goto-char`                 |                      | [avy](https://github.com/abo-abo/avy)              |
| `avy-goto-line`                 |                      | [avy](https://github.com/abo-abo/avy)              |
| `counsel-M-x`                   |                      | [swiper](https://github.com/abo-abo/swiper)        |
| `counsel-find-file`             |                      | [swiper](https://github.com/abo-abo/swiper)        |
| `handy-expand-region-dwim`      |                      | [customized](./src/inside-emacs-6/part-6/utils.el) |
| `handy-mark-dwim`               |                      | [customized](./src/inside-emacs-6/part-6/utils.el) |
| `handy-mark-line`               |                      | [customized](./src/inside-emacs-6/part-6/utils.el) |
| `insight-scroll-up-half-window` |                      | [customized](./src/inside-emacs-6/part-6/utils.el) |
| `org-cycle`                     | TAB                  | built-in                                           |
| `org-set-property`              | C-c C-x p            | built-in                                           |
| `org-shiftmetaright`            | M-S-<right>          | built-in                                           |
| `org-table-export`              |                      | built-in                                           |
| `ta-describe-thing-at-point`    |                      | [customized](./src/inside-emacs-6/part-6/utils.el) |
| `ta-org-shiftmetadown`          |                      | [customized](./src/inside-emacs-6/part-6/utils.el) |

</details>

Video grabbed with my [emacs configuration](https://github.com/tonyaldon/emacs.d)
at commit `06ba16e26da4fbedb430090287aec096bf491037`.

## Inside Emacs 6 (part 7)

[Watch on youtube](https://www.youtube.com/watch?v=LogbcVWb3mQ)

<details>
  <summary>Emacs Lisp</summary>

| functions/variables         | docstring                                             |
|-----------------------------|-------------------------------------------------------|
| `orgtbl-to-generic`         | Convert the orgtbl-mode TABLE to some other format.   |
| `orgtbl-to-html`            | Convert the orgtbl-mode TABLE to HTML.                |
| `org-export-define-backend` | Define a new back-end BACKEND.                        |
| `org-export-options-alist`  | Alist between export properties and ways to set them. |

</details>

<details>
  <summary>Commands</summary>

| commands                        | default key bindings     | packages                                             |
|---------------------------------|--------------------------|------------------------------------------------------|
| `avy-goto-char`                 |                          | [avy](https://github.com/abo-abo/avy)                |
| `avy-goto-line`                 |                          | [avy](https://github.com/abo-abo/avy)                |
| `counsel-M-x`                   |                          | [swiper](https://github.com/abo-abo/swiper)          |
| `counsel-yank-pop`              |                          | [swiper](https://github.com/abo-abo/swiper)          |
| `eval-last-sexp`                | C-x C-e                  | built-in                                             |
| `handy-expand-region-dwim`      |                          | [customized](./src/inside-emacs-6/part-7/utils.el)   |
| `handy-mark-dwim`               |                          | [customized](./src/inside-emacs-6/part-7/utils.el)   |
| `handy-mark-inside-dwim`        |                          | [customized](./src/inside-emacs-6/part-7/utils.el)   |
| `insight-scroll-up-half-window` |                          | [customized](./src/inside-emacs-6/part-7/utils.el)   |
| `isearch-forward`               | C-s                      | built-in                                             |
| `isearch-occur`                 | M-s o (isearch-mode-map) | built-in                                             |
| `narrow-to-region`              | C-x n n                  | built-in                                             |
| `org-edit-special`              | C-c ' (org-mode-map)     | built-in                                             |
| `org-edit-src-exit`             | C-c ' (org-mode-map)     | built-in                                             |
| `org-table-export`              |                          | built-in                                             |
| `recenter-top-bottom`           | C-l                      | built-in                                             |
| `sp-next-sexp`                  |                          | [smartparens](https://github.com/Fuco1/smartparens)  |
| `ta-avy-goto-end-of-line`       |                          | [customized](./src/inside-emacs-6/part-7/utils.el)   |
| `ta-describe-thing-at-point`    |                          | [customized](./src/inside-emacs-6/part-7/utils.el)   |
| `yas-expand`                    |                          | [yasnippet](https://github.com/joaotavora/yasnippet) |

</details>

Video grabbed with my [emacs configuration](https://github.com/tonyaldon/emacs.d)
at commit `06ba16e26da4fbedb430090287aec096bf491037`.

## Inside Emacs 6 (part 8)

[Watch on youtube](https://www.youtube.com/watch?v=JyG54FFWu-o)

<details>
  <summary>Emacs Lisp</summary>

| functions/variables            | docstring                                              |
|--------------------------------|--------------------------------------------------------|
| `orgtbl-radio-table-templates` | Templates for radio tables in different major modes.   |
| `advice-add`                   | Like ‘add-function’ but for the function named SYMBOL. |
| `inhibit-message`              | Non-nil means calls to ‘message’ are not displayed.    |
| `indent-region`                | Indent each nonblank line in the region.               |
|                                |                                                        |

</details>

<details>
  <summary>Commands</summary>

| commands                             | default key bindings          | packages                                           |
|--------------------------------------|-------------------------------|----------------------------------------------------|
| `counsel-M-x`                        |                               | [swiper](https://github.com/abo-abo/swiper)        |
| `orgtbl-mode`                        |                               | built-in                                           |
| `orgtbl-hijacker-command-4`          | M-S-<right> (orgtbl-mode-map) | built-in                                           |
| `orgtbl-hijacker-command-100`        | <return>    (orgtbl-mode-map) | built-in                                           |
| `orgtbl-hijacker-command-102`        | <tab>       (orgtbl-mode-map) | built-in                                           |
| `orgtbl-hijacker-command-17`         | S-<return>  (orgtbl-mode-map) | built-in                                           |
| `orgtbl-hijacker-command-109`        | <backspace> (orgtbl-mode-map) | built-in                                           |
| `orgtbl-ctrl-c-ctrl-c`               | C-c C-c     (orgtbl-mode-map) | built-in                                           |
| `org-table-transpose-table-at-point` |                               | built-in                                           |
| `orgtbl-insert-radio-table`          |                               | built-in                                           |
| `recenter-top-bottom`                | C-l                           | built-in                                           |
| `ta-avy-goto-end-of-line`            |                               | [customized](./src/inside-emacs-6/part-8/utils.el) |
| `counsel-yank-pop`                   |                               | [swiper](https://github.com/abo-abo/swiper)        |

</details>

Video grabbed with my [emacs configuration](https://github.com/tonyaldon/emacs.d)
at commit `06ba16e26da4fbedb430090287aec096bf491037`.

## Inside Emacs 6 (part 9)

[Watch on youtube](https://youtu.be/wrEYankhAIs)

<details>
  <summary>Commands</summary>

| commands                  | default key bindings       | packages                                           |
|---------------------------|----------------------------|----------------------------------------------------|
| `handy-mark-line`         |                            | [customized](./src/inside-emacs-6/part-9/utils.el) |
| `org-ctrl-c-ctrl-c`       | C-c C-c  (orgtbl-mode-map) | built-in                                           |
| `org-yank`                | C-y      (orgtbl-mode-map) | built-in                                           |
| `ta-avy-goto-end-of-line` |                            | [customized](./src/inside-emacs-6/part-9/utils.el) |

</details>

Video grabbed with my [emacs configuration](https://github.com/tonyaldon/emacs.d)
at commit `15379cdd5e548f1540d677d4386bb5da7d5bc5b0`.

# Great video series

I've learnt a lot of emacs cool stuff from [magnars](https://github.com/magnars) and
[protesilaos](https://protesilaos.com/about/). Go for it:

* [emacs rocks](http://emacsrocks.com/),
* [protesilaos (youtube)](https://www.youtube.com/channel/UC0uTPqBCFIpZxlz_Lv1tk_g).

# Contact

Do you have any question or suggestion? Please, feel free to:
* leave me a message on twitter <a
href="http://www.twitter.com/tonyaldon">@tonyaldon</a>
* or to email me at tony.aldon.adm@gmail.com.

# License
Project under MIT license
