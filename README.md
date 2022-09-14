# dotfiles

What began as a simple repository for my personal Bash/Zsh dotfiles has become a
full-on development environment installation project. The name "dotfiles" is a
bit deceptive, as this repository contains far more than configuration files.
"Pluggable UNIX development environment" may be more fitting to the soul of this
project. Shell user profile files such as `.bashrc` and `.zshrc` act as
entry-points to the (mostly) shell-agnostic configuration buried within this
repository's folders. What emerges from sourcing `.bashrc` or `.zshrc` is a set
of aliases, functions, and wrappers that allow me to act as I please on any
UNIX-based operating system.

## IMPORTANT NOTE (PLEASE READ)

This project is highly opinionated -- once the user runs `install.sh`, their
development environment will be replaced entirely by my own custom environment.
As such, I doubt this project's configuration is suited for most users, and I
urge anyone interested in my setup to comb through my files and take only what
they find useful. **Run `install.sh` at your own risk.** In fact, don't run it
at all.

## Background

I view my dotfiles repository as an extension of my keyboard. I've grown and
adapted my configuration for many years, and it has gotten to the point where
doing without these files would be very difficult for me. It is a bit like my
interactive programming notebook. I began compiling my dotfiles in 2015 while
working at IBM. At the time, I had a good friend and mentor who taught me all
about Bash and the UNIX command line. My friend maintained a large, customized
dotfiles repository. I had never heard of dotfiles before, nor of the convention
whereby developers collected and shared them, but I was intrigued. The fact that
my friend could open a terminal on a brand new computer, run a few POSIX
commands, and have his established programming environment fully functional in
no time flat was simply astounding to me. It was then that I started putting
together my own dotfiles project, and growing it has been one of the happiest
hobbies of my adult life.

I do hope that the automated approach I have taken to bootstrapping a UNIX-based
development environment inspires other developers to do the same. I cannot
adequately convey the amount of time and effort this repository has saved me
over the years in terms of configuration and coding.

## Usage

1. Read through my files,
1. Find my many strange and terrifying shell functions,
1. Grab your chin with your thumb and forefinger and say "Hmm, that really makes
   you think!",
1. Copy whatever you want for whatever reason,
1. Do not, under any circumstances, run `install.sh`,
1. Enjoy!

Seriously, I designed this project for my own needs, so I have to advise others
**not** to install this project on their computers. It will bamboozle and
bewilder you, probably won't work even as I intend, and may permanently destroy
your existing development configuration. If you have questions about anything in
this project, please don't hesitate to open a ticket. I always value other
developers' feedback.

## To Rich Hickey, should he be reading this

Hi. Can I work at your company? I know Clojure really well, and I don't like
`HttpServletRequest` ;)
