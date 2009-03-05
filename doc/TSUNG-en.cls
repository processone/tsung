%% $Id :$

\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{TSUNG-en}[2002/12/02 0.1]

\DeclareOption*{\PassOptionsToClass{\CurrentOption}{article}}
\ProcessOptions
\LoadClass[10pt]{article}

\RequirePackage[colorlinks=false]{hyperref}
\RequirePackage{url}
\RequirePackage{xspace}

\RequirePackage[T1]{fontenc}
%\RequirePackage[cyr]{aeguill}
%\RequirePackage{ae}
\RequirePackage{a4wide}
\RequirePackage{fancyhdr}
\RequirePackage{lastpage}
\RequirePackage{color}
\RequirePackage{colortbl}           % Cf. _LaTeX par la pratique_, p. 300...
\RequirePackage{nopageno}
% \RequirePackage[latin1]{inputenc}
% \RequirePackage{ucs}
\RequirePackage[utf8]{inputenc}
%\RequirePackage[francais,english]{babel}
\RequirePackage[english]{babel}
\RequirePackage{tabularx}
\RequirePackage{hhline}
\RequirePackage{textcomp}
\RequirePackage{times}
%\RequirePackage[english]{varioref}   % références en français
\RequirePackage[debugshow,final]{graphics}

\RequirePackage{fancyvrb} % paquetage verbatim évolué
\fvset{fontsize=\scriptsize,numbers=left,frame=single}

\newcommand{\GreyShade}{0.95}
\newcommand{\GrayShade}{\GreyShade}

\newif\ifpdf
\ifx\pdfoutput\undefined
   \pdffalse              % we are not running PDFLaTeX
\else
   \pdfoutput=1           % we are running PDFLaTeX
   \pdftrue
\fi

\ifpdf
  \RequirePackage[pdftex]{graphicx}
  \pdfcompresslevel=9
  \DeclareGraphicsExtensions{.png,.jpg,.pdf,.mps}
\else
  \RequirePackage{graphicx}
  \DeclareGraphicsExtensions{.ps,.eps}
\fi

%%
\setlength{\voffset}{-.4in}
%\setlength{\headheight}{14.5mm}
\setlength{\headheight}{50pt}
\setlength{\textheight}{215mm}

\definecolor{idxgreen}{rgb}{0.22,0.50,0.18}%#39812F
%% Headers


%% Headers
\pagestyle{fancy}
\renewcommand{\sectionmark}[1]{\markboth{\thesection.\ #1}{}}
\renewcommand{\subsectionmark}[1]{\markright{#1}}
\rhead{\emph{\leftmark\\\rightmark}}
\lhead{\logo{2}}
\rfoot{\small{Page \thepage\//\pageref{LastPage}}}
%\rhead{\leftmark}
\cfoot{\scriptsize {Tsung Users manual: \url{http://tsung.erlang-projects.org}}}
\renewcommand{\footrulewidth}{0.2pt}
\lfoot{}

%% same thing without subsection
\fancypagestyle{fancytoc}{%
  \fancyhf{}
  \rhead{\emph{\leftmark}}
  \lhead{\logo{2}\\%
    \it{\textcolor{idxgreen}{Tsung} manual}}
  \cfoot{\scriptsize {Tsung Users manual: \url{http://tsung.erlang-projects.org}}}
}

%% change fancy headers for toc: 
\let\myTOC\tableofcontents
\renewcommand{\tableofcontents}{%
  \begingroup
  \let\ps@plain\ps@empty
  \pagestyle{fancytoc}
  \myTOC
  \clearpage
  \endgroup%
}

%% 
%% Build title page
\renewcommand{\@maketitle}{%
%  \newpage
%  \null
%FIXME : Need to define a minipage to be sure changing page after maketitle
%\begin{minipage}[t][0.95\textheight][t]{\textwidth}
% \usebox{\addressbox}%         
%       \vspace*{1cm}
    \begin{flushright}%
      \fontsize{14}{18}\selectfont%
      \usebox{\titlebox} \\%
%        \usebox{\numberbox}\\
%        \usebox{\datebox}
%        \usebox{\authorbox}\\[2ex]%
    \end{flushright}%

    \vspace*{\fill}
    \begin{tabular}{p{20mm} r}
      & \includegraphics[scale=0.7]{logo_tsung} \\
    \end{tabular}
    \vspace*{\fill}

    \CartoucheGeneral
    \thispagestyle{plain}
%     \newpage


%     \begin{flushleft}%
%         \usebox{\doccopyrightbox}\\%
%     \end{flushleft}%
%     \vspace{2cm}
%     \begin{flushleft}%
%     {\huge\textbf{Written by:}}
%     \begin{minipage}[t]{\textwidth}%
%       \usebox{\authorbox}\\[2ex]%
%     \end{minipage}
%     \end{flushleft}%
%     \vspace{2cm}
%     \par

%\end{minipage}
}

\newcommand{\CartoucheGeneral}{%
  \begin{center}
  \newcolumntype{Y}{>{\columncolor[gray]{\GrayShade}}r}
  \begin{tabular}{|Yl|}
  \hline
%   Version:          & \verb@version \DocumentVersion{}      \\
   Version:          & \idxversion       \\
   References:        & \idxref    \\
%   Date de Publication: & \idxreldate{}  \\
   Printing Date: & \today \\
  \hline 
  \end{tabular}
  \end{center}
}

\newcommand{\logo}[1]{%
\includegraphics[width=30mm]{logo_tsung}
}


% Boxes for the title
\newsavebox{\authorbox}
\savebox{\authorbox}{}
\newsavebox{\titlebox}
\newsavebox{\numberbox}
\newsavebox{\datebox}
\newsavebox{\addressbox}
\newsavebox{\doccopyrightbox}


\savebox{\addressbox}{%
\begin{minipage}[t]{\textwidth}%
         \logo{3}       \hfill  \parbox[c]{5cm}{ 
                                \flushright%
                                OLDSTUFF
                                }

\end{minipage}
}
%

\newcommand{\addauthor}[3]{%
  \savebox{\authorbox}{%
    \parbox[t]{\textwidth}{%
      \flushleft%
      \usebox{\authorbox}\\%
      {\fontfamily{\familydefault}\fontsize{10}{11}%
        \fontseries{m}\fontshape{n}\selectfont%
        #1 }
      {\fontfamily{\familydefault}\fontsize{12}{14}%
        \fontshape{sc}\selectfont%
        #2 }%
      \fontsize{12}{12}\selectfont%
      {\fontfamily{\familydefault}\fontsize{10}{11}%
        \fontseries{m}\fontshape{n}\selectfont%
        (\noexpand\url{#3}})%
      }%
    }
  }
\newcommand{\Abstract}[1]{%
  \begin{flushleft}
  {\huge{\textbf{Abstract:}}}\\
  \end{flushleft}
  \begin{center}
    \vspace{5mm}
  {\em
  \begin{tabular}{|p{1mm}p{120mm}p{1mm}|}
  \hline 
  & & \\
  & #1 & \\
  & & \\
  \hline 
  \end{tabular}
  }
  \end{center}
}

\newcommand{\doctitle}[1]{%
  \savebox{\titlebox}{%
    \parbox[b]{\textwidth}%
%    {\begin{flushright}%
%      {\Huge \textbf{#1}\par}%
%      \Huge{\vspace{2ex}}%
%    \end{flushright}%
%    }%
   {\flushright%
    \rule[0.5ex]{\textwidth}{0.5mm}\\%
    \vspace{5mm}%
%      \fontfamily{\familydefault}\fontsize{18}{22}%
      \fontfamily{\familydefault}\Huge%
      \fontseries{b}\fontshape{n}\selectfont%
      #1 \\%
      \rule[0.5ex]{\textwidth}{0.5mm}%
    }%
  }%
}
\@onlypreamble{\doctitle}

\newcommand{\docref}[1]{%
  \newcommand{\idxref}{\texttt{#1}}
}
\newcommand{\docversion}[1]{%
  \newcommand{\idxversion}{#1}
  \savebox{\numberbox}{
    \fontfamily{\familydefault}\fontsize{14}{18}%
    \fontseries{b}\fontshape{n}\selectfont%
    Version: #1}%
  }

\newcommand{\docreldate}[1]{%
  \newcommand{\idxreldate}{#1}
  \savebox{\datebox}{
    \fontfamily{\familydefault}\fontsize{14}{18}%
    \fontseries{m}\fontshape{n}\selectfont%
    #1}%
  }

\newcommand{\doccopyright}[1]{%
  \savebox{\doccopyrightbox}{%
    {\fontfamily{\familydefault}\fontsize{8}{10}%
      \fontseries{m}\fontshape{n}\selectfont%
      Copyright: {\copyright} Nicolas Niclausse}%
    }%
  }


\renewcommand{\abstract}{
\section*{Résumé}%
}

\newcommand{\docobjectifs}{%
\section{Objectifs}
}

\newcommand{\docperimetre}{%
\section{perimeter}
}




