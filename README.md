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

I heavily rely on [hydra](https://github.com/abo-abo/hydra). I've
broke my `hydras` into 4 kinds off actions :

* `hydra-lines`: actions related to lines and short movements,
* `hydra-sp`: actions perform on `sexp` with
  [smartparens](https://github.com/Fuco1/smartparens),
* `hydra-browse`: actions related to browsing code,
* `hydra-windows`: actions related to window layout.

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

<details>
  <summary>Commands</summary>

| commands                     | packages                                             |
|------------------------------|------------------------------------------------------|
| `avy-goto-word-or-subword-1` | [avy](https://github.com/abo-abo/avy)                |
| `counsel-M-x`                | [avy](https://github.com/abo-abo/avy)                |
| `delete-blank-lines`         | built-in                                             |
| `describe-variable`          | built-in                                             |
| `eval-defun`                 | built-in                                             |
| `eval-region`                | built-in                                             |
| `mark-paragraph`             | built-in                                             |
| `next-buffer`                | built-in                                             |
| `previous-buffer`            | built-in                                             |
| `revert-buffer`              | built-in                                             |
| `sp-next-sexp`               | [smartparens](https://github.com/Fuco1/smartparens)  |
| `swiper`                     | [counsel](https://github.com/abo-abo/swiper)         |
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

command-remapping

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

command-remapping

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
