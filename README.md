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

# Great video series

I've learnt a lot of emacs cool stuff from [magnars](https://github.com/magnars) and
[protesilaos](https://protesilaos.com/about/). Go for it:

* [emacs rocks](http://emacsrocks.com/),
* [protesilaos (youtube)](https://www.youtube.com/channel/UC0uTPqBCFIpZxlz_Lv1tk_g).

# Inside Emacs 1

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

Video grabbed with my [emacs configuration](https://github.com/tonyaldon/emacs.d)
at commit `f760601bfc92bac7570f74396dbe1e4910af86af`.

# Inside Emacs 2

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

Video grabbed with my [emacs configuration](https://github.com/tonyaldon/emacs.d)
at commit `f760601bfc92bac7570f74396dbe1e4910af86af`.

# Contact

Do you have any question or suggestion? Please, feel free to:
* leave me a message on twitter <a
href="http://www.twitter.com/tonyaldon">@tonyaldon</a>
* or to email me at tony.aldon.adm@gmail.com.

# License
Project under MIT license
