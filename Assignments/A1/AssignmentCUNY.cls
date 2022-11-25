%% AssignmentCUNY
%% Copyright 2022 Alex Washburn

\LoadClass[%
parskip=half,           % spacing value / method for paragraphs
chapterprefix=true,     % prefix for chapter marks
10pt,                   % font size
headings=normal,        % size of headings
bibliography=totoc,     % include bib in toc
listof=totoc,           % include listof entries in toc
listof=numberline,      % include listof entries in toc
listof=nochaptergap,
titlepage=on,           % own page for each title page
captions=tablebelow,    % display table captions above the float env
chapterprefix=false,    % do not display a prefix for chapters
appendixprefix=false,   % but display a prefix for appendix chapter
draft=false,            % value for draft version
toc=flat,
toc=numberline,
numbers=noenddot
]{scrreprt}%

\NeedsTeXFormat{LaTeX2e}
\ProvidesPackage{AssignmentCUNY}[2022/06/10 v1.0.0 A LaTeX style CUNY assignments]
%\PassOptionsToPackage{utf8}{inputenc}
%\usepackage{inputenc}
\RequirePackage{geometry}
\geometry{%
a4paper,
bottom=20mm,
left=20mm,
right=20mm,
top=20mm
}


%----------------------------- CUNY Assignment variables -------------------------
\global\let\VarAssignmentNumber\empty%
\global\let\VarCourseName\empty%
\global\let\VarCourseNumber\empty%
\global\let\VarStudentName\empty%
\global\let\VarStudentNumber\empty%

\global\let\VarAssignmentNumber\relax
\global\let\VarCourseName\relax
\global\let\VarCourseNumber\relax
\global\let\VarStudentName\relax
\global\let\VarStudentNumber\relax

\def\AssignmentNumber#1{\def\VarAssignmentNumber{#1}}
\def\CourseName#1{\def\VarCourseName{#1}}
\def\CourseNumber#1{\def\VarCourseNumber{#1}}
\def\StudentName#1{\def\VarStudentName{#1}}
\def\StudentNumber#1{\def\VarStudentNumber{#1}}
%----------------------------- CUNY Assignment variables -------------------------


\RequirePackage[dvipsnames,svgnames,hyperref]{xcolor}
\KOMAoptions{twoside=false}

\DeclareTOCStyleEntry[beforeskip=-0.1in]{chapter}{chapter}
\DeclareTOCStyleEntry[beforeskip=-0.1in]{section}{section}
\DeclareTOCStyleEntry[beforeskip=-0.1in]{default}{subsection}
\DeclareTOCStyleEntry[beforeskip=-0.1in]{default}{subsubsection}
%----------------------------- Setup table of contents -------------------------


%----------------------------- Modify paragraph format -------------------------
\newlength\savedparfillskip%
\newcommand*{\saveparfillskip}[1]{\setlength{\savedparfillskip}{\dimexpr\parfillskip\relax}}
\AddtoDoHook{heading/postinit/chapter}{\saveparfillskip}
\renewcommand{\chapterlinesformat}[3]{%
	\ctchaptertitle{#3}%
	#2%
	\hspace{-\savedparfillskip}%
}
%----------------------------- Modify paragraph format -------------------------


%----------------------------- Other packages ----------------------------------
\usepackage{latexsym}
\usepackage{lmodern}
\usepackage{setspace}
\renewcommand{\familydefault}{\sfdefault}
%----------------------------- Other packages ----------------------------------


%----------------------------- Update sectioning -------------------------------
\usepackage{longfbox}
\usepackage{changepage}

\renewcommand{\sectionlinesformat}[4]{%
\@tempswafalse
\Ifstr{#1}{section}{%
\begin{adjustwidth}{-17mm}{-17mm}%
\hspace*{#2}%
\begin{longfbox}[
border-width=1mm,
margin=0pt,
padding-top=6mm,
padding-right=16mm,
padding-left=16mm,
padding-bottom=6mm
]
\raggedsection#3~\\[2mm]\Large\textbf{#4}%
\end{longfbox}%
\end{adjustwidth}%
}{\hangfrom{\hskip #2#3}{#4}}
}

\addtokomafont{disposition}{\normalfont}
\addtokomafont{part}{\scshape\LARGE}
\addtokomafont{chapter}{\scshape\Huge}
\addtokomafont{section}{\sffamily\bfseries\Large}
\addtokomafont{subsection}{\bfseries}
\addtokomafont{subsubsection}{\bfseries}
\addtokomafont{paragraph}{\scshape\small}
\addtokomafont{subparagraph}{\scshape\small}

\renewcommand*{\thesection}{\textsc{\textsf{\LARGE Problem~\arabic{section}:}}}
\renewcommand*{\thesubsection}{\arabic{section}~(\alph{subsection}):}
\renewcommand*{\thesubsubsection}{\arabic{section}~(\alph{subsubsection})~(\Roman{subsubsection}):}
\RedeclareSectionCommands[beforeskip=-3ex,afterskip=2ex]{section,subsection,subsubsection}
%----------------------------- Update sectioning -------------------------------


%--------------------------------- Internal commands ---------------------------
\newcommand{\CourseTitle}{CSc~\VarCourseNumber~--~\VarCourseName}
\newcommand{\AssignmentTitle}{Assignment~\textnumero\VarAssignmentNumber}
\newcommand{\HomeworkText}[1]{\textbf{``#1''}}

\renewcommand{\maketitle}{%
\thispagestyle{empty}
\parindent=0pt
\KOMAoptions{fontsize=12pt}
\begin{center}
\singlespacing%
~\vfill
\Huge\CourseTitle\\[1cm]
\AssignmentTitle\normalsize\\[1cm]
by\\[1cm]
\Large\VarStudentName\normalsize\\[1cm]
on\\[1cm]
\Large\today\\[1cm]%
~\vfill
\end{center}
\KOMAoptions{fontsize=10pt}
}
%--------------------------------- Internal commands ---------------------------


%--------------------------------- Exported Commands ---------------------------
\newcommand{\CoverPage}{%
\begin{titlepage}
% Always use Arabic numbering, even in Front-matter
\setcounter{page}{1}
\pagenumbering{arabic}
\maketitle%
\doublespacing%
\end{titlepage}
}

\newcommand{\Problem}[1]{%
\clearpage\newgeometry{bottom=20mm,left=20mm,right=20mm,top=3mm}%\afterpage{\restoregeometry}
\section{#1}}%
\newcommand{\SubProblem}[1]{\subsection{\HomeworkText{#1}}}%
\newcommand{\SubSubProblem}[2]{\subsubsection{\HomeworkText{#1}}}%

