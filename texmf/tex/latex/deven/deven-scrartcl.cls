\NeedsTeXFormat{LaTeX2e}[1994/06/01]
\ProvidesClass{deven-scrartcl}[2017/03/02 Deven custom macros for scrartcl]

\DeclareOption{default}{
  \PassOptionsToClass{12pt, letterpaper, twoside}{scrartcl}
}

\DeclareOption*{
  \PassOptionsToClass{\CurrentOption}{scrartcl}
}

\ExecuteOptions{default}

\ProcessOptions\relax

\LoadClass{scrartcl}

\RequirePackage{deven}
\RequirePackage{scrlayer-scrpage}

\setkomafont{disposition}{\normalfont\scshape}
\setkomafont{section}{\normalsize}
\ihead{\textnormal{\ligatured \large \@author, \@title\ifthenelse{\equal{\@sectname}{}}{}{, part \@sectname},} \ligatured page \thepage~of \pageref{\@sectname end}}
\cfoot{}\ifoot{}\ofoot{}

\newcommand{\sectname}[1]{\renewcommand{\@sectname}{#1}}
  \newcommand{\@sectname}{}

\AtEndDocument{
  \label{\@sectname end}
  \ifx\@sectenum\undefined
  \else
    \end{enumerate}
  \fi
}

\newcommand{\beforenextsect}[1]{\newcommand{\@beforenextsect}{#1}}

\newcommand{\sect}[1]{
  \label{\@sectname end}
  \cleardoublepage
  \setcounter{page}{1}
  \ifx\@beforenextsect\undefined
  \else
    \@beforenextsect
    \let\@beforenextsect\relax
  \fi
  \sectname{#1}
}

\renewcommand{\thesection}{\arabic{section}}
\newcommand{\sectheader}[2][]{
  \sect{\thesection}
  \section{
    \hspace{-1ex}
    \ifx&#1&
    \else
      |\hspace{1ex}#1\hspace{0.7ex}
    \fi
    \ifx&#2&
    \else
      |\hspace{1ex}#2
    \fi
  }
}

\newcommand{\sectenum}[2][]{
  \ifx\@sectenum\undefined
    \newcommand{\@sectenum}{}
    \sect{#2}
    \ifx&#1&
      \begin{enumerate}[series=sectenum]
    \else
      \begin{enumerate}[#1,series=sectenum]
    \fi
  \else
    \end{enumerate}
    \sect{#2}
    \ifx&#1&
      \begin{enumerate}[resume*=sectenum]
    \else
      \begin{enumerate}[#1,resume=sectenum]
    \fi
  \fi
}

\endinput
