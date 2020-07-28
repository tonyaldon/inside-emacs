# About
`inside-emacs` is a top-up for the video serie **inside emacs**.

Watch [](here).

# Content

If you want to reproduce some of the actions made in **inside
emacs** videos, here you'll find:

1. Various list of the commands I use in the videos and the packages
   they belong to,
2. The code of the customized commands I use in the videos (I put them
   in `utils.el` file in the corresponding subdirectories of the
   [./src/](src) directory), they generaly are prefixed by `ta-`
   (stands for Tony Aldon),
3. When it's possible, the files I use to make the videos (see the
   [./src/](src) directory).

# Emacs "keybinding" paradigm

I've rebound mostly every standard keybindings. I've not thought about
the general use of these bindings, I just wanted them to fit the
specific [https://github.com/tonyaldon/keyboard-layout](keyboard-layout)
I'm using.

I heavily rely on [https://github.com/abo-abo/hydra](hydra). I've
broke my `hydras` into 4 kinds off actions :

* `hydra-lines`: actions related to lines and short movements,
* `hydra-sp`: actions perform on `sexp` with
  [https://github.com/Fuco1/smartparens](smartparens),
* `hydra-browse`: actions related to browsing code,
* `hydra-windows`: actions related to window layout.

For more details, you can browse my
[https://github.com/tonyaldon/emacs.d](emacs configuration).

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

I've learnt a lot of emacs cool stuff from [https://github.com/magnars](magnars) and
[https://protesilaos.com/about/](protesilaos). Go for it:

* [http://emacsrocks.com/](emacs rocks),
* [https://www.youtube.com/channel/UC0uTPqBCFIpZxlz_Lv1tk_g](protesilaos
  (youtube)).

# Contact

Do you have any question or suggestion? Please, feel free to:
* leave me a message on twitter <a
href="http://www.twitter.com/tonyaldon">@tonyaldon</a>
* or to email me at tony.aldon.adm@gmail.com.

# License
Project under MIT license
