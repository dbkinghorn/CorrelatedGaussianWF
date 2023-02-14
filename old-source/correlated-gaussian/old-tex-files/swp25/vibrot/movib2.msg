%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                     %
%              Scientific Word   Wrap/Unwrap  Version 2.5             %
%                                                                     %
% If you are separating the files in this message by hand, you will   %
% need to identify the file type and place it in the appropriate      %
% directory.  The possible types are: Document, DocAssoc, Other,      %
% Macro, Style, Graphic, PastedPict, and PlotPict. Extract files      %
% tagged as Document, DocAssoc, or Other into your TeX source file    %
% directory.  Macro files go into your TeX macros directory. Style    %
% files are used by Scientific Word and do not need to be extracted.  %
% Graphic, PastedPict, and PlotPict files should be placed in a       %
% graphics directory.                                                 %
%                                                                     %
% Graphic files need to be converted from the text format (this is    %
% done for e-mail compatability) to the original 8-bit binary format. %
% The following C code will do this:                                  %
%                                                                     %
%     void main()                                                     %
%     {                                                               %
%       unsigned char src_byte = 0;                                   %
%       int src_mask  =  1;                                           %
%       unsigned char dst_byte = 0;                                   %
%       int dst_mask  =  1;                                           %
%                                                                     %
%       while ((src_byte = getchar()) != EOF) {                       %
%         if (src_byte < 64)  continue; // ignore whitespace          %
%         src_mask = 1;                                               %
%                                                                     %
%         while (src_mask < 64) {                                     %
%           if (src_byte & src_mask)  dst_byte |= dst_mask;           %
%                                                                     %
%           src_mask = src_mask << 1;   /* increment src_mask */      %
%           dst_mask = dst_mask << 1;   /* increment dst_mask */      %
%                                                                     %
%           if (dst_mask == 256) {                                    %
%             putchar(dst_byte);                                      %
%             dst_byte = 0; dst_mask = 1;                             %
%           }                                                         %
%         }                                                           %
%       }                                                             %
%     }                                                               %
%                                                                     %
% Put the code in a file called "convert.c" and compile it. For       %
% example, if you are using gcc compiler you would type:              %
%                                                                     %
%     gcc convert.c -o convert                                        %
%                                                                     %
% If "source" is the name of your source graphic file and "dest" is   %
% to be the name of the converted graphic file then you would type:   %
%                                                                     %
%     convert < source  > dest                                        %
%                                                                     %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                     %
% Files included:                                                     %
%                                                                     %
% "\document\movib2.tex", Document, 51084, 2/19/1997, 2:27:07, ""     %
% "\document\movib2.bbl", DocAssoc, 6445, 2/19/1997, 2:27:54, ""      %
%                                                                     %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%% Start \document\movib2.tex %%%%%%%%%%%%%%%%%%%%%

%% This document created by Scientific Word (R) Version 2.5
%% Starting shell: mathart1


\documentclass[12pt,thmsa]{article}
\usepackage{amsfonts}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\usepackage{sw20aip, doublesp}

%TCIDATA{Created=Tue Oct 08 16:55:55 1996}
%TCIDATA{LastRevised=Tue Feb 18 17:22:59 1997}
%TCIDATA{Language=American English}

\input{tcilatex}
\begin{document}

\author{Donald B. Kinghorn and Ludwik Adamowicz \\
%EndAName
Dept. of Chemistry, University of Arizona, Tucson, AZ 85721}
\title{A New N-Body Potential and Basis Set for Adiabatic and Non-adiabatic
Variational Energy Calculations }
\date{2 January, 1997}
\maketitle

\begin{abstract}
A new functional form for multi-body expansions of potential energy surfaces
and basis functions for correlated adiabatic and fully non-adiabatic
variational energy calculations is presented. N-body explicitly correlated
Gaussians with pre-multiplying factors consisting of products of powers of
internal distance coordinates are utilized in a dual role to analytically
represent isotropic potentials and energy eigen-functions in the same
internal coordinate system. Practical aspects of this new methodology are
presented. The ideas and methods are prototyped and illustrated with two
simple diatomic examples; the Morse potential and an accurate $H_2$
potential for which essentially exact results are obtained for vibrational
energy levels.
\end{abstract}

\section{Introduction}

Advances in theoretical quantum chemistry coupled with the phenomenal
increase in the computational power of computers has opened the door for the
development of ``\emph{ab initio} spectroscopy and dynamics.'' Using
advanced computational methodologies such as coupled cluster methods\cite
{Bartlett95}, it is now possible to generate accurate potential energy
surfaces for molecular systems with even a dozen or more atoms. However,
there is still much work to be done in developing methods for representing
these surfaces analytically to facilitate dynamics calculations. Also, even
though there have been many successes in computing vibration-rotation energy
levels for di-atomic and tri-atomic systems, the problem of computing
general n-body vibration-rotation energy levels remains largely unsolved.
Another problem, one that is almost entirely untouched, is that of computing
molecular energy levels fully non-adiabatically, \textit{i.e.}, solving the
Schr\"{o}denger equation, including all electronic and nuclear degrees of
freedom, with fully correlated wave functions. The present work proposes a
new coherent methodology addressing these problems.

\subsection{Analytic representations of potential surfaces}

There have been three primary methods employed for analytically representing
potential energy surfaces: series expansions, interpolating schemes, and
many-body function representation. The power series approach includes the
early work of Dunham\cite{Dunham32a,Dunham32b} using expansions in $%
(R-R_e)/R_e$ and more recent work by Simons, Parr, and Finlan\cite{Simons73}
using $(R-R_e)/R$, Ogilvie\cite{Ogilvie81} using $2\left( R-R_e\right)
/\left( R+R_e\right) $, Thakkar\cite{Thakkar75} with $1-(R_e/R)^{-a-1}$ and
Huffaker\cite{Huffaker76} using an expansion with a Morse\cite{Morse29}-like
variable $1-\exp \left[ -a\left( R-R_e\right) \right] ,$ ($a$ is a Dunham
constant). Better convergence radii can be obtained using rational
polynomials such as the Pad\'{e} approximants used in the study of several
di-atomic systems\cite{Jordan74,Jorish79,Sonnleitner81,Pardo86}. Another
type of expansion is to use a Taylor series obtained by numerical
differentiation to give a quadratic or quartic force field representation.
Lee, Martin, Taylor, Dateo, Schwenke and Fran\c{c}ois have obtained very
good results for low lying vibrational levels using accurate quartic force
fields\cite{Lee95a,Dateo94,Martin93,Lee95b} for systems as large as ethylene%
\cite{Martin95}. There is a hybrid approach using Morse functions and two
dimensional splines suggested by Koizumi, Schatz and Walch\cite{Koizumi91}
which has been described and used by Bentley, Boman, Gazdy, Lee and Dateo%
\cite{Bentley92}. This method is similar to the rotated Morse oscillator
method\cite{Bowman75,Connor75}. Interpolation schemes using splines or
piecewise polynomials have been used for fitting single and
multi-dimensional potential energy surfaces\cite
{Forsythe77,Malik80,Sathyamurthy75,Dunne87,Bruehl88}. The many-body function
representation as been explored most thoroughly by Murrell, Carter,
Farantos, Huxley, and Varandas\cite{Murrell84}. The Sorbie-Murrell\cite
{Sorbie75} potential for $H_2O$ was used successfully by Whitehead and Handy%
\cite{Whitehead76} for vibrational band origin calculations. Vegiri and
Farantos\cite{Vergiri88} fitted an $HeOH$ potential surface using the
many-body description given by Carter and coworkers\cite{Carter82}. There
obviously has been numerous descriptions in the literature of analytic
representations of potential surfaces. The reader is referred to the
monographs by Searles and von-Nagy-Felsobuki\cite{Searles93}, Murrell and
coworkers\cite{Murrell84}, and Mezey\cite{Mezey87} for more information.

\subsection{Energy level calculations}

There seems to be nearly as many methods for computing vibration-rotation
energy levels as there are workers in the field. Nearly all of the
references in the previous section contain descriptions of, or references
to, methods for computing, at least, low lying vibrational energy levels;
therefore, those references will not be repeated here. Historically, methods
for investigation of vibration-rotation energy levels have ranged from
simple harmonic oscillator rigid rotor models with perturbation extensions
based on the classical rovibrational Hamiltonians of Eckart\cite{Eckart35},
Willison and Howard\cite{WilsonHoward36}, and the quantum mechanical version
of the Eckart Hamiltonian put forth by Watson\cite{Watson68}, to the
variational method of Sutcliffe and Tennyson\cite{SutcliffeTennyson87}, and
the Morse oscillator based Hamiltonian, (MORBID), of Jensen\cite{Jensen88}.
There are successful programs for numerically computing energy levels such
as Robert LeRoy's program LEVEL 6.0\cite{LeRoy95} for di-atomic systems and
Jeremy Hutson's program BOUND\cite{Hutson93} for numerical 2- or 3-D
calculations of atom-diatom Van der Waals molecules. Most methodologies
utilize complicated coordinate and Hamiltonian transformations and/or
approximations which are specific for the systems investigated and are not
easily generalized. The method we propose does not require exotic
transformations or approximations of the Hamiltonian, is conceptually
straight forward, and is easily extended to larger systems.

\subsection{Our methodology}

The centerpiece of our methodology is the multiple use of functions of the
general form, 
\begin{equation}
\phi _k=\prod_{i<j}r_{ij}^{m_{kij}}\exp \left[ -\mathbf{r}^{\prime
}(A_k\otimes I_3)\mathbf{r}\right] .  \label{basisfcn}
\end{equation}
These functions, hereafter referred to simply as ``$\phi _k$'', will serve
as multi-parameter expansion functions for analytically representing
potential energy surfaces and, with the addition of a rotational component
and appropriate symmetry projection, will be used as variational basis
functions for vibration-rotation (and electronic) wave functions. In $\phi _k
$ the term $\prod_{i<j}r_{ij}^{m_{kij}}$ is a product of ``distance''
coordinates, $r_{ij},$ raised to powers $m_{kij}$ (positive negative or
zero). The exponential component is an explicitly correlated Gaussian with $%
\mathbf{r}$ representing a length $3n$ column vector of internal (relative)
coordinates ($\mathbf{r}^{\prime }$ denotes the transpose of $\mathbf{r,}$ 
\textit{i.e.} a row vector), $A_k$ is an $n\times n$ symmetric matrix of
exponent parameters (usually positive semi-definite). The Kronecker product
of $A_k$ with the $3\times 3$ identity matrix, $I_3$, insures rotational
invariance as will be shown in a later section. The matrix/vector form of $%
\phi _k$ allows us to exploit the powerful matrix differential calculus,
described by Kinghorn\cite{Kinghorn95a}, for deriving elegant and easily
implementable mathematical forms for integrals and derivatives required in
variational calculations.

The first proposed use for $\phi _k$ we will discuss is as expansion
functions for n-body potential energy surfaces or, in general, any potential
which is an isotropic function of internal coordinates. Our expansion will
utilize $\phi _k$ in the spirit of the many-body expansion described by
Murrell\cite{Murrell84}. Note: The n-particle coulomb potential is a special
case of this expansion: Set $A_k=0$ and choose the appropriate $m_{kij}=-1$%
or $0$. Also note that Lennard-Jones type potentials are also special cases
of an expansion in $\phi _k.$

Utilizing $\phi _k$ as expansion functions for the potential energy operator
and as variational basis functions, both in the same internal coordinates $%
\mathbf{r}$, gives us a simple, coherent method for computing energy
eigenstates without making unnecessary transformations and approximations to
the Hamiltonian. This flexibility in the $\phi _k$ will allow our procedures
to be used unmodified for both n-body vibration-rotation calculations with
analytic representations of adiabatic potential energy surfaces and for
completely correlated, fully non-adiabatic energy calculations.
(Non-adiabatic calculations are necessarily restricted to few electron
systems because of the requirement of explicit symmetry projection for Pauli
allowed states). Note: The $\phi _k$ are angular momentum eigenfunctions
with eigenvalue $J=0$; thus, modification of the basis to $Y_M^L\phi _k$
will yield higher angular momentum eigen-states when the factor $Y_M^L$ is
chosen as an angular momentum eigenfunction for the desired state. These
uses of $\phi _k$ will be discussed in the sections that follow. We begin
with a discussion of coordinates, and the general physical model, \textit{%
i.e.}, the Hamiltonian.

\section{ Coordinates and the Hamiltonian}

The general problem being consided is to find the discrete energy levels of
a system of particles interacting under an isotropic potential. More
specifically, we are interested in two cases: Case 1, where the particles
are atoms in a molecule or in a cluster and the potential is modeled by a
many-body expansion in $\phi _k$ using a least-squares fit to adiabatic 
\textit{ab-initio} energy calculations and/or experimental data. In this
case vibrational and rotational energies are obtained. Case 2, where the
particles are nuclei and electrons with a coulombic potential. In this case,
the energy levels include all electronic, vibrational and rotational
interactions. Both of these cases are modeled by the same general form of
the Hamiltonian.

To model the physical systems, \textit{i.e.}, write the Hamiltonian,
particles are considered to be non-relativistic, charged, point masses
interacting under an isotropic potential. The total Hamiltonian then has the
familiar form, 
\begin{equation}
H_{tot}=-\sum_i^N\frac{\nabla _{\mathbf{R}_i}^2}{2M_i}+V\left( \left\| 
\mathbf{R}_i-\mathbf{R}_j\right\| \,;\,\,\,i<j,\,\,\,\,i=1...N\right) .
\label{ham1}
\end{equation}
The particles are numbered from $1$ to $N$ with $M_i$ the mass of particle $i
$, $\mathbf{R}_i=[X_i\,\,Y_i\,\,Z_i]^{\prime }$ a column vector of Cartesian
coordinates for particle $i$ in the external, laboratory fixed, frame, $%
\nabla _{\mathbf{R}_i}^2$ the Laplacian in the coordinates of $\mathbf{R}_i$%
, and $\left\| \mathbf{R}_i-\mathbf{R}_j\right\| $ the distance between
particles $i$ and $j$. The total Hamiltonian, eqn(\ref{ham1}), is, of
course, separable into an operator describing the translational motion of
the center of mass and an operator describing the internal energy. This
separation is realized by a transformation to center-of-mass and internal
(relative) coordinates which are now presented.

Let $\mathbf{R}$ be the vector of particle coordinates in the laboratory
fixed reference frame. 
\begin{equation}
\mathbf{R=}\left[ 
\begin{array}{c}
\mathbf{R}_1 \\ 
\mathbf{R}_2 \\ 
\vdots  \\ 
\mathbf{R}_N
\end{array}
\right] =\left[ 
\begin{array}{c}
X_1 \\ 
Y_1 \\ 
Z_1 \\ 
\vdots  \\ 
Z_N
\end{array}
\right] 
\end{equation}
Center of mass and internal coordinates are given by the transformation $T:%
\mathbf{R}\mapsto [\mathbf{r}_0^{\prime },\mathbf{r}^{\prime }]^{\prime }$ 
\begin{equation}
T=\left[ 
\begin{array}{ccccc}
\frac{M_1}{m_0} & \frac{M_2}{m_0} & \frac{M_3}{m_0} & \cdots  & \frac{M_N}{%
m_0} \\ 
-1 & 1 & 0 & \cdots  & 0 \\ 
-1 & 0 & 1 & \cdots  & 0 \\ 
\vdots  & \vdots  & \vdots  & \ddots  & \vdots  \\ 
-1 & 0 & 0 & \cdots  & 1
\end{array}
\right] \otimes I_3  \label{Ttran}
\end{equation}
where $m_0=\sum_i^NM_i$. $\mathbf{r}_0$ is the vector of coordinates for the
center-of-mass and $\mathbf{r}$ is a length $3n=3\left( N-1\right) $ vector
of internal coordinates with respect to a reference frame with origin at
particle 1 
\begin{equation}
\mathbf{r=}\left[ 
\begin{array}{c}
\mathbf{r}_1 \\ 
\mathbf{r}_2 \\ 
\vdots  \\ 
\mathbf{r}_n
\end{array}
\right] =\left[ 
\begin{array}{c}
\mathbf{R}_2-\mathbf{R}_1 \\ 
\mathbf{R}_3-\mathbf{R}_1 \\ 
\vdots  \\ 
\mathbf{R}_N-\mathbf{R}_1
\end{array}
\right] .  \label{rdef}
\end{equation}
Using this coordinate transformation, and the conjugate momentum
transformation, the internal Hamiltonian for the problems we are considering
(cases 1 and 2 above) can be written as 
\begin{equation}
H=-\frac 12\left( \sum_i^n\frac 1{\mu _i}\nabla _i^2+\sum_{i\neq j}^n\frac
1{M_1}\nabla _i\cdot \nabla _j\right) +V\left(
r_{ij};\,\,\,i<j,\,\,\,\,\,i=0...n\right) ,  \label{intham1}
\end{equation}
where the $\mu _i$ are reduced masses, $M_1$ is the mass of particle 1, (the
coordinate reference particle), and $\nabla _i$ is the gradient with respect
to the $x,y,z$ coordinates $\mathbf{r}_i$. The potential energy is still a
function of the distance between particles but is now written using internal
distance coordinates, $r_{ij}=\left\| \mathbf{r}_i-\mathbf{r}_j\right\| =$ $%
\left\| \mathbf{R}_{i+1}-\mathbf{R}_{j+1}\right\| \,\,$with\thinspace $%
r_{0j}\equiv r_j=\left\| \mathbf{r}_j\right\| =\left\| \mathbf{R}_{j+1}-%
\mathbf{R}_1\right\| .$ The kinetic energy term in this Hamiltonian can be
written as a quadratic form in the length $3n$ vector gradient operator, $%
\nabla _{\mathbf{r}},$ the gradient with respect to the length $3n$ vector $%
\mathbf{r}$ of internal coordinates. This gives a compact matrix/vector form
of the Hamiltonian with the kinetic energy expressed as a quadratic form in
the gradient operator, 
\begin{equation}
H=-\nabla _{\mathbf{r}}^{\prime }\left( M\otimes I_3\right) \nabla _{\mathbf{%
r}}+V\left( r_{ij};\,\,\,i<j\,,\,\,\,\,i=0...n\right)   \label{ham}
\end{equation}
$M$ is an $n\times n$ matrix with $1/2\mu _i$ on the diagonal and $1/2M_1$
for off-diagonal elements. This is the Hamiltonian we use for variational
energy calculations (case 1 and case 2). We make no further transformations
or approximations to this Hamiltonian. More information on the
center-of-mass separation and form of the Hamiltonian, eqn(\ref{ham}), can
be found in the references\cite{Kinghorn93,Kinghorn95b}.

Our n-body potential expansion function, $\prod_{i<j}r_{ij}^{m_{kij}}\exp
\left[ -\mathbf{r}^{\prime }(A_k\otimes I_3)\mathbf{r}\right] ,$ is written
using the scalar ``distance'' variables $\left\{ r_{ij}\right\} $ and the
internal coordinate vector variable $\mathbf{r.}$ These two sets of
variables both  describe the ``geometry'' of a system. To make this
equivalence more obvious and to present alternative forms for the $\phi _k,$
we will show the interchangability of these two sets of variables. $r_{ij}^m$
can be written as a function of $\mathbf{r}$ using the matrix $\left(
J_{ij}\otimes I_3\right) $ with $J_{ij}$ defined as an $n\times n$ matrix
with 1's in the $ii$ and $jj$ positions, -1 in the $ij$ and $ji$ positions
and 0's elesewhere\cite{Poshusta83,Kinghorn95a}, 
\begin{equation}
r_{ij}^m=\left[ \mathbf{r}^{\prime }(J_{ij}\otimes I_3)\mathbf{r}\right]
^{m/2}.  \label{rijJ}
\end{equation}
$r_{ij}^m$ can, equivalently, be written using the component vectors, $%
\mathbf{r}_i$, of $\mathbf{r}$ 
\begin{eqnarray}
r_{ij}^m &=&\left[ \mathbf{r}_i^{\prime }\mathbf{r}_i+\mathbf{r}_j^{\prime }%
\mathbf{r}_j-2\mathbf{r}_i^{\prime }\mathbf{r}_j\right] ^{m/2} \\
&=&\left[ r_i^2+r_j^2-2\mathbf{r}_i^{\prime }\mathbf{r}_j\right] ^{m/2}.
\end{eqnarray}
Thus, using eqn(\ref{rijJ}), $\phi _k$ can be written purely in terms of the
vector variable $\mathbf{r}$, 
\begin{equation}
\phi _k=\prod_{i<j}\left[ \mathbf{r}^{\prime }(J_{ij}\otimes I_3)\mathbf{r}%
\right] ^{\frac{m_{kij}}2}\exp \left[ -\mathbf{r}^{\prime }(A_k\otimes I_3)%
\mathbf{r}\right] .  \label{phir}
\end{equation}

Alternatively, $\phi _k$ can be expressed using the distance coordinates $%
\left\{ r_{ij}\right\} $. The quadratic form in the exponential of $\phi _k$
may be converted to $\left\{ r_{ij}\right\} $ variables as follows, (we drop
the subscript $k$ for convenience), 
\begin{eqnarray}
\mathbf{r}^{\prime }(A\otimes I_3)\mathbf{r} &=&\sum_{i,j}\mathbf{r}%
_i^{\prime }\mathbf{r}_j\,\,A_{ij} \\
&=&\func{tr}\left[ \left( \mathbf{r}_i^{\prime }\mathbf{r}_j\right) A\right] 
\\
&=&\func{tr}\left[ \left( r_{ij}^2\right) B\right]  \\
&=&\sum_{i,j}r_{ij}^2\,\,B_{ij}
\end{eqnarray}
where $\func{tr}\left[ \,\,\right] $ is the matrix trace operator, $\left( 
\mathbf{r}_i^{\prime }\mathbf{r}_j\right) $ is the $n\times n$ matrix of dot
products of the component vectors of $\mathbf{r},$ $\left( r_{ij}^2\right) $
is the $n\times n$ matrix of squared distance variables, and $B$ is a matrix
with elements given, in terms of the elements of an arbitrary matrix $A$, by
the transformation, 
\begin{equation}
B_{ij}=\left\{ 
\begin{array}{ll}
\frac 12\dsum\limits_{k=1}^n\left( A_{ik}+A_{kj}\right) , & i=j \\ 
-\frac 14\left( A_{ij}+A_{ji}\right) , & i\neq j
\end{array}
\right. .  \label{Btran}
\end{equation}
Hence, the $\phi _k$ can be written using only distance coordinates, 
\begin{equation}
\phi _k=\prod_{i<j}r_{ij}^{m_{kij}}\exp \left[ -\func{tr}\left[ \left(
r_{ij}^2\right) B_k\right] \right] .
\end{equation}
If one has $\phi _k$ in terms of $B_k$, \textit{i.e.}, a function of $r_{ij},
$ and wishes to transform to $A_k,$ a function of $\mathbf{r}$, the
following relation can be used, 
\begin{equation}
A_{ij}=\left\{ 
\begin{array}{ll}
B_{ii}+\dsum\limits_{k\neq i,j}^n\left( B_{ik}+B_{kj}\right) , & i=j \\ 
-\left( B_{ij}+B_{ji}\right) , & i\neq j
\end{array}
\right. .  \label{Atran}
\end{equation}
Note that even though $A_k$ and consequently $B_k$, are symmetric, we leave
the transformations, eqn(\ref{Atran}) and eqn(\ref{Btran}), in general form
for clarity.

Another point to make about $A_k$, the matrix of the quadratic form in $\phi
_k$: if $\phi _k$ is to be square integratable, \textit{i.e.,} admit a norm,
then $A_k$ must be positive definite. The simplest way to insure this
condition is to write $A_k$ in Cholesky factored form, 
\begin{equation}
A_k=L_kL_k^{\prime }\text{ ,\thinspace \thinspace \thinspace \thinspace
\thinspace \thinspace \thinspace \thinspace }L_k\text{ lower triangular and
rank }n.
\end{equation}
Positive definiteness of $A_k$ is required when the $\phi _k$ are used as
basis function for variataional energy calculations. When $\phi _k$ are used
to model potential energy surfaces this restriction on $A_k$ is not
required. However, most physically meaningful potentials will require $A_k$
to be positive semi-definite, the exceptions being when some type of boundry
is desired in the form $\phi _k\rightarrow \infty $ as some $%
r_{ij}\rightarrow \infty .$

As a final comment on coordinates consider an $N$ particle system, requiring
only, in general,  $3N-6$ coordinates to specify a ``geometry''. The vector $%
\mathbf{r}$ is composed of $n=N-1$ component vectors for a total of $3n=3N-3$
coordinates. This is because $\mathbf{r}$ contains 3 degrees of freedom
associated with rotation about the origin even though the functions $\phi _k$
are rotationaly invariant. Also, if the distance coordinates, $\left\{
r_{ij}\right\} ,$ are considered, then it should be noted that there are $%
n\left( n+1\right) /2=N\left( N-1\right) /2$ of them. Hence, for $N>4,$ the $%
r_{ij}$ are linearly dependent since $N\left( N-1\right) /2>3N-6$ when $N$
is greater than 4. Also, when $N$ is greater than 6, then $N\left(
N-1\right) /2>3N-3.$ These relations are shown explicitly in Table \ref{Ntab}%
. These differences in the number of coordinates do not present any real
problems since transformations between variables $\mathbf{r}$, $\left\{
r_{ij}\right\} $ and ``z-matrix'' type coordinates is straight forward and
all three representations describe the same ``geometry'' without ambiguity.

%TCIMACRO{\TeXButton{B}{\begin{table}[tbp] \centering}}
%BeginExpansion
\begin{table}[tbp] \centering%
%EndExpansion
\begin{tabular}{cccc}
\hline\hline
& $\mathbf{r}$ & $\left\{ r_{ij}\right\} $ &  \\ 
$N$ & $3N-3$ & $N\left( N-1/2\right) $ & $3N-6$ \\ \hline
2 & 3 & 1 & 1$\,\,(3N-5)$ \\ 
3 & 6 & 3 & 3 \\ 
4 & 9 & 6 & 6 \\ 
5 & 12 & 10 & 9 \\ 
6 & 15 & 15 & 12 \\ 
7 & 18 & 21 & 15 \\ 
8 & 21 & 28 & 18 \\ \hline
\end{tabular}
\caption{Number of coordinate variables for {\bf{r}} and $ r_{ij} $
\label{Ntab}}%
%TCIMACRO{\TeXButton{E}{\end{table}}}
%BeginExpansion
\end{table}%
%EndExpansion

\section{N-Body Potential}

Consider the suitability of $\phi _k$ as an expansion function for potential
energy hypersurfaces. There are several requirements that a general n-body
potential needs to satisfy:

\begin{enumerate}
\item  It should be invariant to rotations of the system, \textit{i.e.,}
isotropic.

\item  It should collapse to a suitable (n-m)-body potential as any m
particles are removed from the system.

\item  The function should approach infinity as any two like particles
approach each other.

\item  The function should be flexible enough to handle complicated behavior
such as multiple maxima and minima.

\item  The function should be differentiable.

\item  There should be a way to constrain the model potential to the
physical symmetries of the system.
\end{enumerate}

\noindent As will be shown below, an expansion in $\phi _k$ can satisfy all
of these requirements.

\subsubsection{Rotational invariance}

Using the vector form, eqn(\ref{phir}), it is easy to show that $\phi _k$ is
rotationally invariant, that is, invariant to any orthogonal transformation.
Let $U$ be any $3\times 3$ orthogonal matrix (any proper or improper
rotation in 3 space) then the action of $U$ on $\phi _k$ is to transform the
quadratic forms in the pre-multiplying factors and exponential factor as
(using the exponential factor as an example), 
\begin{eqnarray}
\left( \left( I_n\otimes U\right) \mathbf{r}\right) ^{\prime }(A_k\otimes
I_3)\left( I_n\otimes U\right) \mathbf{r} &=&\mathbf{r}^{\prime }\left(
I_n\otimes U^{\prime }\right) (A_k\otimes I_3)\left( I_n\otimes U\right) 
\mathbf{r} \\
&=&\mathbf{r}^{\prime }(A_k\otimes U^{\prime }U)\mathbf{r} \\
&=&\mathbf{r}^{\prime }(A_k\otimes I_3)\mathbf{r,}
\end{eqnarray}
leaving $\phi _k$ invariant. Hence, any expansion in $\phi _k$ will be
isotropic in $\Bbb{R}^3$.

\subsubsection{Asymptotic behavior $r_{ij}\rightarrow \infty \,\,\,\,$(The
many-body expansion)}

To ensure the correct asymptotic behavior of the potential as any particles
are adiabatically removed from the system, the many-body expansion described
by Murrell and co-workers\cite{Murrell84} can be used. In this expansion the
potential is written as a sum of $M$-body terms with $M$ ranging from 1 to $N
$ for an $N$ particle system, 
\begin{equation}
V=\sum_{\left\{ A\right\} }V_A+\sum_{\left\{ AB\right\}
}V_{AB}+\sum_{\left\{ ABC\right\} }V_{ABC}+\cdots +V\stackunder{N}{_{%
\underbrace{ABCD\ldots }}}
\end{equation}
$V_A^{\left( 1\right) }$ is the energy of particle $A$ in the state that
would result by adiabatically removing it from the system. The remaining
terms are the appropriate $M$-body terms for $M$ from 2 to $N$. There are $%
\binom NM=\frac{N!}{M!\left( N-M\right) !}$ $M$-body terms in an $N$
particle system. This is best illustrated with a simple example. Consider a
system of three atoms labeled $A,\,B,\,C$, so that the distance from
particle $A$ to particle $B$ is $r_1$, $A$ to $C$ is $r_2$ and $B$ to $C$ is 
$r_{12}$. The model potential can then be written as, 
\begin{eqnarray}
V &=&V_A+V_B+V_C+V_{AB}+V_{AC}+V_{BC}+V_{ABC}  \nonumber \\
&=&V_A+V_B+V_C  \nonumber \\
&&+\sum c_i^{AB}r_1^{m_i^{AB}}e^{-B_i^{AB}r_1^2}+\sum
c_i^{AC}r_2^{m_i^{AC}}e^{-B_i^{AC}r_2^2}+\sum
c_i^{BC}r_{12}^{m_i^{BC}}e^{-B_i^{BC}r_{12}^2}  \nonumber \\
&&+\sum c_i^{ABC}r_1^{m_i^{ABC}}r_2^{n_i^{ABC}}r_{12}^{p_i^{ABC}}\exp \left[
-\mathbf{r}^{\prime }\left( A_i^{ABC}\otimes I_3\right) \mathbf{r}\right] 
\label{MbodyV}
\end{eqnarray}
where $V_A+V_B+V_C$ is the sum of the energy of the separated atoms, the
next three terms are expansions for the possible two-body terms and the last
term is an expansion for the full three-body term. This potential has the
correct asymptotic behavior as long as the exponent parameters are positive
definite, ($A_l$ is a $3\times 3$ positive definite symmetric matrix of
exponent parameters and, trivially, the $B$'s are $1\times 1$ positive
definite matrices).

\subsubsection{Asymptotic behavior $r_{ij}\rightarrow 0$}

The correct asymptotic behavior of the potential as any $r_{ij}$ approach
zero is ensured by including at least one negative value of the exponent $m$
in each of the two-body expansions when an expansion such as eqn(\ref{MbodyV}%
) is used. This gives the condition, $V\rightarrow \infty $ as $%
r_{ij}\rightarrow 0.$

\subsubsection{Flexability of $\phi _k$}

It is not expected that an expansion in $\phi _k$ will result in the most
compact analytic model function possible for a given potential, but rather
that the $\phi _k$ will provide the flexibility needed to model complex
dynamical behavior, perhaps by using a large number of the $\phi _k$ in an
expansion. A general $\phi _k$ term in an $n$-body component of an expansion
will contain one linear parameter and $n\left( n+1\right) /2$ non-linear
parameters and many terms could be used to represent this component.
Additionally, for each term there are the powers, $m_{ij},$ for the $r_{ij}$
pre-multiplying factors, to be chosen. The number of possible
pre-multiplying factors for an $n$-body term with $Z$ different values of
the powers $m$ is given by, 
\begin{equation}
\text{\# of terms }\left( \prod_{i<j}r_{ij}^{m_{kij}}\right) \text{ }%
=Z^{n\left( n+1\right) /2}.
\end{equation}
For example, there would be 4096 possible pre-multiplying factors for a
4-body term if all possible values of $m_{ij}$ including -1, 0 ,1 and 2 are
used. (Obviously one would not want to use all of these.) Thus, there could
be a large number of parameters available for fitting the model potential to 
\emph{ab-initio} and/or experimental data. This gives the model potential
using $\phi _k$ the advantage of great flexibility at the expense of,
possibly, difficult optimization of the many parameters. The point being ---
the $\phi _k$ provide a large pallet of terms for modeling a potential by
non-linear data fitting, but these terms will obviously need to chosen
carefully. This is the art of non-linear curve fitting.

\subsubsection{Derivatives}

The $\phi _k$ are infinitely differentiable. Therefore, any potentials
modeled as linear combinations of $\phi _k$ are also infinitely
differentiable and expressible in terms of the derivatives of the $\phi _k$.
Derivatives of the potential can be useful for fitting to, or predicting,
experimental force field data and for imposing fitting constraints based on
equilibrium geometries. Derivatives are also useful for characterizing
features of the potential such as local extrema and saddle points. Treating $%
\phi _k$ as a function of the vector of relative coordinates, $\mathbf{r,}$
as in eqn(\ref{phir}), the gradient vector and Hessian matrix can be derived
using matrix differential calculus\cite{Kinghorn95a}. Introducing an
over-bar notation for the Kronecker product with the $3\times 3$ identity, 
\textit{i.e.,} $A_k\otimes I_3=\bar{A}_k\,\,$, and defining $G=\left(
\sum_{i\leq j}m_{kij}\left( \mathbf{r}^{\prime }\bar{J}_{ij}\mathbf{r}%
\right) ^{-1}\bar{J}_{ij}\right) $), the gradient with respect to $\mathbf{r}
$ is then, 
\begin{equation}
\nabla _{\mathbf{r}}\phi _k=\left[ G-2\bar{A}_k\right] \phi _k\,\mathbf{r,}
\end{equation}
and the Hessian is given by, 
\begin{eqnarray}
\frac{\partial \nabla _{\mathbf{r}}\phi _k}{\partial \mathbf{r}^{\prime }}
&=&\left[ G+\left[ G-2\bar{A}_k\right] \mathbf{rr}^{\prime }\left[ G-2\bar{A}%
_k\right] -2\bar{A}_k\mathstrut \strut 
\begin{array}{ll}
&  \\ 
& 
\end{array}
\right.   \nonumber \\
&&\left. -2\sum_{i\leq j}m_{ij}\left( \mathbf{r}^{\prime }\bar{J}_{ij}%
\mathbf{r}\right) ^{-2}\bar{J}_{ij}\mathbf{rr}^{\prime }\bar{J}_{ij}\right] .
\end{eqnarray}

Two other derivatives which may be useful when fitting a potential are the
derivatives with respect to the non-linear parameters in the matrices $A_k$
and $B_k$. These matrices are symmetric and therefore contain only $n\left(
n+1\right) /2$ independent parameters which we represent using the operator $%
\func{vech}\left[ \,\,\right] $. $\func{vech}\left[ A\right] $ is a column
vector of the lower triangular elements of a matrix $A.$ (See references for
notation.\cite{Kinghorn95a,Kinghorn95b}) The gradient of $\phi _k$ with
respect to $\func{vech}\left[ A_k\right] $ is, 
\begin{equation}
\nabla _{\func{vech}\left[ A_k\right] }\phi _k=-\phi _k\func{vech}\left[ 2%
\mathbf{rr}^{\prime }-\func{diag}\left[ \mathbf{rr}^{\prime }\right] \right]
,
\end{equation}
where $\mathbf{rr}^{\prime }$ is an $n\times n$ rank one matrix formed from
the vector $\mathbf{r}$ and $\func{diag}\left[ \mathbf{rr}^{\prime }\right] $
is the matrix $\mathbf{rr}^{\prime }$ with all off-diagonal elements set to
zero. The gradient of $\phi _k$ with respect to $\func{vech}\left[
B_k\right] $ is given by, 
\begin{equation}
\nabla _{\func{vech}\left[ B_k\right] }\phi _k=-\phi _k\func{vech}\left[
2\left( r_{ij}^2\right) -\func{diag}\left[ \left( r_{ij}^2\right) \right]
\right] .
\end{equation}

\subsubsection{Symmetry}

A meaningful model potential will necessarily reflect the physical symmetry
of the system being modeled. Rotational symmetry has already been addressed
(the $\phi _k$ are isotropic), leaving the problem of how to handle
permutational symmetry. The potential should be totally symmetric with
respect to exchange of like particles. This permutational symmetry can be
accounted for in the $\phi _k$ using a projection method. Consider a system
of $N$ particles invariant under the action of a group, $G$, represented by
a set, $\left\{ P_{\alpha \in G}\right\} ,$ of $N\times N$ permutation
matrices. A model potential represented as an expansion in $\phi _k$ is a
function of the $n=N-1$ component vectors of $\mathbf{r}$, the relative
coordinates. The permutation $P_\alpha $ acting on the $N$ particle
coordinates induces a transformation on the center-of-mass and relative
coordinates given by, 
\begin{equation}
T\bar{P}_\alpha T^{-1}=I_3\oplus \bar{\tau}_\alpha ,
\end{equation}
where $T$ is the transformation matrix given in eqn(\ref{Ttran}). The right
hand side of this expression is the direct sum of the identity acting on the
center-of-mass coordinates, $\mathbf{r}_0$, and $\tau _\alpha ,$ which is an 
$n\times n$ ``permutation'' matrix acting on the component vectors of the
relative coordinate vector $\mathbf{r}$. The action of the permutation
represented by $P_\alpha $ on $\phi _k$ is then, 
\begin{equation}
P_\alpha \phi _k=\prod_{i<j}\left[ \mathbf{r}^{\prime }(\tau _\alpha
^{\prime }J_{ij}\tau _\alpha \otimes I_3)\mathbf{r}\right] ^{\frac{m_{kij}}%
2}\exp \left[ -\mathbf{r}^{\prime }(\tau _\alpha ^{\prime }A_k\tau _\alpha
\otimes I_3)\mathbf{r}\right] .
\end{equation}
The action of the totally symmetric representation of the group $G$ on $\phi
_k$ is thus induced by the projector $\sum_{\alpha \in G}\tau _\alpha .$
This method of symmetry projection on correlated Gaussians is discussed in
more detail in the references\cite{Poshusta83,Kinghorn93,Kinghorn95b}.

\section{Variational Energy Calculations}

We have in mind two types of variational energy calculations utilizing $\phi
_k$: Case 1, vibration-rotation calculations, and case 2, fully
non-adiabatic energy calculations. In both cases the wave functions will be
expanded as symmetry projected linear combinations of the explicitly
correlated $\phi _k$ multiplied by an angular term, $Y_{LM}^k.$ 
\begin{equation}
\Psi _{LM\Gamma }=\mathcal{P}_\Gamma \sum_kY_{LM}^k\phi _k.  \label{wf}
\end{equation}
Here $\phi _k$ are the explicitly correlated n-body Gaussians give in eqn(%
\ref{basisfcn}), $\mathcal{P}_\Gamma $ is an appropriate permutational
symmetry projection operator for the desired state, $\Gamma $, and $Y_{LM}^k$
is a product of coupled solid harmonics labeled by the total angular
momentum quantum numbers $L$ and $M$.

Permutational symmetry is handled using projection methods in the same
manner as described for the potential expansion in the previous section.
Again, the reader is refered to the references for details\cite
{Poshusta83,Kinghorn93,Kinghorn95b}.

$Y_{LM}^k$ is a vector coupled product of solid harmonics\cite{Biedenharn81}
given by the Clebsch Gordon expansion, 
\begin{equation}
Y_{LM}^k=\sum_{\,\,\QATOPD. . {\left\{ l_j,m_j\right\} }{m_1+\cdots
+m_n=M}}\left\langle LM;k\right. |\left. l_1m_1\cdots l_nm_n\right\rangle
\prod_j^n\mathcal{Y}_{l_jm_j}
\end{equation}
The solid harmonics are given by, 
\begin{equation}
\mathcal{Y}_{lm}\left( \mathbf{r}_j\right) =\left[ \frac{2l+1}{4\pi }\left(
l+m\right) !\,\left( l-m\right) !\right] ^{\frac 12}\sum_p\frac{\left(
-x_j-iy_j\right) ^{p+m}\left( x_j-iy_j\right) ^pz_j^{l-2p-m}}{2^{2p+m}\left(
p+m\right) !\,p!\,\left( l-m-2p\right) !}.
\end{equation}
The $\mathcal{Y}_{lm}\left( \mathbf{r}_j\right) $ are single particle
angular momentum eigen-functions in relative coordinates which transform the
same as spherical harmonics, \textit{i.e.} have the same eigen-values. Since
the $\phi _k$ are angular momentum eigen-functions with zero total angular
momentum, the product with $Y_{LM}^k$ can be used in principle to obtain any
desired angular momentum eigen-state. Note the $k$ dependence of $Y_{LM}^k;$
this is included since there are many ways to couple the individual angular
momentum $l_j$ to achieve the desired total angular momentum $L$ and it may
be necessary to include several sets of the $l_j$ in order to obtain a
realistic description of the wave function. Varga and Suzuki\cite{Varga95}
have proposed representing the angular dependence of the wave function using
a single solid harmonic whose argument contains additional variational
parameters. There appears to be several advantages in doing this and we are
investigating the possibility of using this approach in our full N-body
implementation.

There have been several highly accurate non-adiabatic variational
calculations on atomic and exotic few particle systems using simple
correlated Gaussians\cite
{Kinghorn93,Kinghorn95b,Kozlowski93b,Kinghorn96a,Varga96}. By simple we mean
they only contain the exponential part of the $\phi _k,$ (no $r_{ij}$
pre-multipliers). However, attempts at non-adiabatic molecular calculations
have been plagued by problems with linear dependence in the basis during
energy optimizations. This problem occurs in calculations on atomic systems
also, but to a much lesser extent. We believe that we understand this
phenomenon and that our new basis including pre-multiplying powers of $r_{ij}
$ will eliminate or at least drastically reduce the linear dependence
problems. Our reasoning is as follows: In systems with more than one heavy
particle there will be large particle density away from the origin in
relative coordinates. That is, the wave function will have peaks shifted
away from the origin. There are three ways to account for this behavior in
the wave function using correlated Gaussians: 1) use correlated Gaussians
with shifted centers, \textit{i.e.}, $\exp [-\left( \mathbf{r-s}\right)
^{\prime }\bar{A}\left( \mathbf{r-s}\right) ];$ 2) Use near linearly
dependent combinations of simple correlated Gaussians with large matched $%
\pm $ linear coefficients; or 3) Use pre-multiplying powers of $r_{ij}$. The
first option is unacceptable since it results in a wave function which no
longer represents a pure angular momentum state. The second option is what
we believe causes the linear dependence and numerical instability which we
are trying to avoid. The third option is what we are proposing. The linear
dependence that we have observed in our calculations using the simple
correlated Gaussians looks, in some sense, like an attempt by the
optimization to include in the wave function derivatives of the basis
functions with respect to the non-linear parameters. The near linear
dependent terms resemble numerical derivatives. Removal of these near linear
dependent terms has an adverse affect on the wave functions, as manifest by
poor energy results, but leaving them in leads to numerical instabilities
which hinder optimization or cause complete collapse of the eigen-solutions.
Now, derivatives of simple Gaussians with respect to non-linear parameters,
elements of the matrices $A_k$, bring down pre-multiplying (even) powers of $%
r_{ij}$. Thus, explicitly including pre-multiplying powers of $r_{ij}$ in
the basis functions should add the needed flexibility to the basis in a
numerically stable way. Also, we expect the rate of convergence to be
improved by these pre-multiplying $r_{ij}^m$ terms in the same way that they
effect convergence in the Hyllerass basis. The $\phi _k$ are similar to the
Hyllerass basis functions with the Slater type exponentials replaced by
fully correlated Gaussian type exponentials. We believe that excellent
results for both case 1 and case 2 type calculations will be obtained using
our new variational basis functions. We have preliminary results to support
the above assertions which are now presented.

\section{An Illustrative Example}

We will provide a simple example of using $\phi _k$ as expansion functions
for potentials and as variational vibrational energy basis functions for a
Morse potential and the $H_2$ potential of Kolos and Wolniewicz\cite{Kolos65}%
. The purpose of these calculations was to test the ideas and general
procedure for using $\phi _k$. Angular factors where not included in these
trial calculations and thus, since the $\phi _k$ are angular momentum
eigen-functions with total angular momentum equal to zero, the results are
for ground and excited vibrational levels in the rotational ground state.
The computer code that was generated for fitting the potential and then
computing variational eigen-values was developed only for testing and is
restricted to use for diatomic systems. We wanted to rapidly develop a
simple prototype to evaluate efficacy of the procedure before proceeding
with full implementation. Happily the prototype worked well and we will be
proceeding with full implementation of the n-body code. A paper describing
implementation details and formulas for matrix elements will appear
separately.

The general procedure for obtaining vibrational energies involves three
steps: 1) obtain a high quality numerical potential, 2) fit the potential to
an expansion in $\phi _k,$ and 3) solve the eigen problem using $\phi _k$ as
basis functions for the vibrational wave functions. This procedure was
followed for the Morse and $H_2$ test cases.

\subsection{Morse Test Case}

A Morse potential was chosen as the first test case. This choice gave us an
exactly solvable model to compare with. The form of the potential was 
\begin{equation}
V_{Morse}\left( r\right) =D_e\left( 1-e^{-a\left( r-r_e\right) }\right)
^2-\varepsilon _{H_2}
\end{equation}
with $D_e=.17449E_h$, $a=1.4556\mathrm{bohr}^{-1}$, $r_e=1.4011\mathrm{bohr}$%
, and $\varepsilon _{H_2}=1.17449E_h.$ A numerical representation of this $%
H_2$ parameterized Morse potential was generated by taking the $\left\{
r,V\left( r\right) \right\} $ data from a plot generated using Mathematica%
\cite{Mathematica} with $r$ ranging from 0 to 100 bohr. This plot gave 242
points distributed more densely where the function was changing most
rapidly, thus giving an excellent numerical representation of the potential.
A least squares procedure was implemented to fit an expansion in the form 
\begin{equation}
V\left( r\right) =c_0+\sum_ic_ir^{m_i}e^{-b_ir^2}
\end{equation}
to the numerical Morse potential. The linear coefficients, $c$'s, were
solved for exactly using a singular value solution of the linear least
squares problem with the non-linear parameters, $b_i$'s, then being
determined iteratively using a quasi-Newton optimization to minimize the
norm of the difference between the vectors $V\left( \mathbf{r}\right) $ and $%
V_{Morse}\left( \mathbf{r}\right) ,$ $\chi $, with $\mathbf{r}$ consisting
of the 242 distance values used to generate the numeric potential, 
\begin{equation}
\min \left[ \chi \right] =\min_{\left\{ b_i\right\} }\left\| V\left( \mathbf{%
r}\right) -V_{Morse}\left( \mathbf{r}\right) \right\| .
\end{equation}
It was possible to generate good fits using just 4 or 5 $\phi _k$ in the
expansion; however, the expansion chosen for the vibrational energy
calculations was rather large consisting of 24 terms, including $\phi _k$
with powers of $r$ ranging from $m=0$, to $m=4$. Note that $m=-1$ is not
needed for the Morse potential since it is finite at $r=0$. This expansion
was essentially exact with $\chi =1.9\times 10^{-9}$. This extremly small
value of $\chi $ is posible because of the smoothness of the numerical data.
Using this expansion as the potential, we variationally solved the
vibrational Schr\"{o}denger equation using $\phi _k$ as basis functions for
the trial wave functions. Our computed vibrational energy levels agree with
Morse's solution of the Schr\"{o}denger equation\cite{Morse29}, with the
largest error occurring in the 12th vibrational level (the highest level).
The largest error was approximately $.05cm^{-1}$, with the average error
being an order of magnitude less than this. Morse's solution of the Schr\"{o}%
denger equation yields a formula for the vibrational eigen-values given by 
\begin{equation}
E_n=\frac{a\hbar }\mu \sqrt{2\mu D_e}\left( n+\frac 12\right) -\frac{%
a^2\hbar ^2}{2\mu }\left( n+\frac 12\right) ^2-\varepsilon .
\end{equation}
This formula gives 12 vibrational energy levels when $H_2$ parameters are
used, ($\mu =918.076341$). Our variational wave functions converged very
rapidly for all 12 levels. For example, a single term wave function gave a
zero point energy in error by only 30 micro-hartrees (6.6$cm^{-1}$), 
\begin{equation}
\Psi _0=Nr^{25}e^{-6.295r^2}
\end{equation}
($N=16.913$ is the normalization factor). At first we were surprised to see
such a large power on $r$, $m=25$, but this is precisely what is needed for $%
\phi _k$ to represent a wave function peaked sharply at a value of $r$
slightly larger than the equilibrium bond length.

\subsection{$H_2$ Test Case}

The second test problem was to determine vibrational energy levels from the
numerical potential for the $^1\sum_g^{+}$ state of Hydrogen computed by
Kolos and Wolniewicz in 1965\cite{Kolos65}. This well-known data set was
chosen because of it's high quality and easy accessibility. The numeric
potential used in our calculations consists of the original 87 points
computed by Kolos and Wolniewicz, in the range .4 to 10 bohr, with an
additional 19 points added in the range 12 to 100 bohr (all set to -1) to
insure the correct asymptotic behavior of the $\phi _k$ expansion during the
fitting procedure. No adiabatic or relativistic corrections where made on
the data. The same least squares procedure used for the Morse data was used
for this numerical $H_2$ potential. An expansion using 15 of the $\phi _k$
with values of the $r$ powers ranging from -1 to 4 yielded an excellent fit
with $\chi =7.5\times 10^{-6}.$ This value of $\chi $ reflects the error,
and thus the lack of smoothness, in the numerical $H_2$ data as compared to
the Morse case.

Variational calculations for the vibrational energy levels were carried out
using our fitted potential and compared with the numerically integrated
results of Wolniewicz\cite{Wolniewicz66}. Agreement between the two
different treatments of the data were very good. Also, with our fitted
potential we were able to compute all of the 15 levels below the
dissociation limit, whereas only the first 13 levels were obtainable with
the numerical integration. Our two highest vibrational levels agree within
two wave numbers of the results obtained by LeRoy\cite{LeRoy68} for the
Kolos and Wolniewicz potential including relativistic and adiabatic
corrections, thus giving us confidence in our result for these highest
levels with the uncorrected data. Table \ref{freqtab} compares our computed
frequencies, $E_{v+1}-E_v$, with those of Wolniewicz\cite{Wolniewicz66}.
Again the variational wave functions converged very rapidly for all 15
levels, with a single term wave function giving a zero point energy in error
by only 15 micro-hartrees (3.3$cm^{-1}$), 
\begin{equation}
\Psi _0=Nr^{17}e^{-4.3496r^2}
\end{equation}
with $N=5.04994$. Here we see a smaller power on $r$ and a smaller exponent
than was the case for the Morse potential. This is a consequence of the
broadening of the $H_2$ potential compared with Morse potential, thus
resulting in a wave function that is peaked less sharply.

%TCIMACRO{\TeXButton{B}{\begin{table}[tbp] \centering}}
%BeginExpansion
\begin{table}[tbp] \centering%
%EndExpansion
\begin{tabular}{cccr}
\hline\hline
$v$ & Wolniewicz & This work & $\nabla $ \\ \hline
\multicolumn{1}{r}{0} & 4163.46 & 4163.44 & -.02 \\ 
\multicolumn{1}{r}{1} & 3927.93 & 3927.94 & .01 \\ 
\multicolumn{1}{r}{2} & 3697.41 & 3697.41 & \TEXTsymbol{<}.01 \\ 
\multicolumn{1}{r}{3} & 3469.51 & 3469.53 & .02 \\ 
\multicolumn{1}{r}{4} & 3242.69 & 3242.66 & -.03 \\ 
\multicolumn{1}{r}{5} & 3014.68 & 3014.77 & .09 \\ 
\multicolumn{1}{r}{6} & 2782.66 & 2782.56 & .10 \\ 
\multicolumn{1}{r}{7} & 2543.24 & 2543.33 & .09 \\ 
\multicolumn{1}{r}{8} & 2292.69 & 2292.73 & .04 \\ 
\multicolumn{1}{r}{9} & 2025.66 & 2025.96 & .30 \\ 
\multicolumn{1}{r}{10} & 1735.53 & 1735.42 & -.11 \\ 
\multicolumn{1}{r}{11} & 1413.65 & 1413.44 & -.21 \\ 
\multicolumn{1}{r}{12} &  & 1047.12 &  \\ 
\multicolumn{1}{r}{13} &  & 618.35 &  \\ \hline
\end{tabular}
\caption{Vibrational frequencies, $ E_{v+1}-E_{v }$,  for $ H_2 $ computed variationaly
 using a fitted potential  and the numerically integrated results o f Wolniewicz.
 All values in $ cm^{-1} $ } \label{freqtab}%
%TCIMACRO{\TeXButton{E}{\end{table}}}
%BeginExpansion
\end{table}%
%EndExpansion

\section{Conclusions}

This work has proposed new methodology for representing potential energy
surfaces analytically and for performing variational energy calculations
utilizing correlated Gaussian functions that have pre-multipliers consisting
of products of distance coordinates raised to variable powers, $%
\prod_{i<j}r_{ij}^{m_{kij}}\exp \left[ -\mathbf{r}^{\prime }(A_k\otimes I_3)%
\mathbf{r}\right] $. These versatile new dual purpose basis functions will
be useful for many-body expansions of potential energy surfaces, as well as
for both adiabatic and non-adiabatic energy calculations. The functions $%
\phi _k$ are flexible enough to meet criteria necessary for global analytic
representation of n-body potential energy surfaces including correct
asymptotic behavior, differentiability, symmetry adaptation, etc.. These
functions can also be used as rapidly convergent variational basis functions
representing wave functions for adiabatic systems to obtain
vibration-rotation energy levels and for fully non-adiabatic energy
calculations utilizing the same procedures for both types of calculations.
We have prototyped our procedures for fitting potential energy surfaces and
for performing variational vibration calculations. The procedures were
effective for the two trial systems investigated, the Morse potential and
the numeric $H_2$ potential of Kolos and Wolniewicz\cite{Kolos65}.

Having successfully completed testing of our general procedures, we will
proceed with full implementation of an n-body program suite. Programs will
include tools to assist in fitting many-body expansions with $\phi _k$ to
numerical potential data and code for n-body variational energy
calculations. Useful tools would include routines for interpolating sparse
data sets and extrapolating to asymptotic limits in order to generate
adequate numerical data for global potential representation and routines for
many parameter linear and non-linear least squares surface fitting. The
n-body code for variational energy calculations utilizing $\phi _k$ as
expansion functions for the potential and as basis functions for the trial
wave functions will be useful for both adiabatic and non-adiabatic
calculations since the coulomb potential is just a special case of a
many-body expansion in $\phi _k$. Integrals needed for matrix elements in
the variational calculations have been completed and work is being done on
more elegant methods for representing high angular momentum states.
Implementation details and formulas for matrix elements will appear in
subsequent work. We hope the present work has shown the feasibility and
flexibility of our methodology and we encourage others to investigate the
usage of our new basis functions, $\phi _k$, especially for analytical
many-body representation of potential energy surfaces.

\bibliographystyle{prsty}
\bibliography{4-prefs,hmhe,mcalc,vibrot}

\end{document}

%%%%%%%%%%%%%%%%%%%%%%% End \document\movib2.tex %%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%% Start \document\movib2.bbl %%%%%%%%%%%%%%%%%%%%%

\begin{thebibliography}{10}

\bibitem{Bartlett95}
R.~J. Bartlett,  in {\em Modern Electronic Structure Theory}, edited by D.~R.
  Yarkony (World Scientific, Singapore, 1995).

\bibitem{Dunham32a}
J.~L. Dunham, Physical Review {\bf 41},  713  (1932).

\bibitem{Dunham32b}
J.~L. Dunham, Physical Review {\bf 41},  721  (1932).

\bibitem{Simons73}
G. Simons, R.~G. Parr, and J.~M. Finlan, The Journal of Chemical Physics {\bf
  59},  3229  (1973).

\bibitem{Ogilvie81}
J.~F. Ogilvie, Proceeding of the Royal Society London A {\bf 378},  287
  (1981).

\bibitem{Thakkar75}
A. Thakkar, The Journal of Chemical Physics {\bf 62},  1693  (1975).

\bibitem{Huffaker76}
J.~N. Huffaker, The Journal of Chemical Physics {\bf 64},  3175  (1976).

\bibitem{Morse29}
P.~M. Morse, Physical Review {\bf 34},  57  (1929).

\bibitem{Jordan74}
K.~D. Jordan, J.~L. Kinsey, and R. Silbey, The Journal of Chemical Physics {\bf
  61},  911  (1974).

\bibitem{Jorish79}
V.~S. Jorish and N.~B. Shcherbak, Chemical Physics Letteres {\bf 67},  160
  (1979).

\bibitem{Sonnleitner81}
S.~A. Sonnleitner, C.~L. Beckel, A.~J. Colucci, and E.~R. Scaggs, The Journal
  of Chemical Physics {\bf 75},  2018  (1981).

\bibitem{Pardo86}
A. Pardo, J.~J. Camacho, and J.~M.~L. Poyato, Chemical Physics Letteres {\bf
  131},  490  (1986).

\bibitem{Lee95a}
T.~J. Lee, J.~M.~L. Martin, and P.~R. Taylor, The Journal of Chemical Physics
  {\bf 102},  254  (1995).

\bibitem{Dateo94}
C.~E. Dateo, T.~J. Lee, and D.~W. Schwenke, The Journal of Chemical Physics
  {\bf 101},  5853  (1994).

\bibitem{Martin93}
J.~M.~L. Martin and T.~J. Lee, Journal of Molecular Spectroscopy {\bf 160},
  105  (1993).

\bibitem{Lee95b}
T.~J. Lee, J.~M.~L. Martin, C.~E. Dateo, and P.~R. Taylor, The Journal of
  Physical Chemistry {\bf 99},  15858  (1995).

\bibitem{Martin95}
J.~M.~L. Martin, T.~J. Lee, P.~R. Taylor, and J.-P. Fran\c{c}ois, The Journal
  of Chemical Physics {\bf 103},  2589  (1995).

\bibitem{Koizumi91}
H. Koizumi and G.~C. Schatz, The Journal of Chemical Physics {\bf 95},  4130
  (1991).

\bibitem{Bentley92}
J.~A. Bentley, J.~M. Bowman, and B. Gazdy, Chemical Physics Letteres {\bf 198},
   563  (1992).

\bibitem{Bowman75}
J.~M. Bowman and A. Kuppermann, Chemical Physics Letteres {\bf 34},  523
  (1975).

\bibitem{Connor75}
J.~N.~L. Connor, W. Jakubetz, and J. Manz, Molecular Physics {\bf 29},  347
  (1975).

\bibitem{Forsythe77}
G.~E. Forsythe, {\em Computer methods for mathematical computations}
  (Prentice-Hall, Englewood Cliffs, N.J., 1977).

\bibitem{Malik80}
D.~J. Malik, J. Eccles, and D. Secrest, Journal of Computational Physics {\bf
  38},  157  (1980).

\bibitem{Sathyamurthy75}
N. Sathyamurthy and L.~M. Raff, The Journal of Chemical Physics {\bf 63},  464
  (1975).

\bibitem{Dunne87}
S.~J. Dunne, D.~J. Searles, and E.~I. {von Nagy-Felsobuki}, Spectrochimica Acta
  {\bf 43A},  699  (1987).

\bibitem{Bruehl88}
M. Bruehl and G. Schatz, The Journal of Physical Chemistry {\bf 92},  7223
  (1988).

\bibitem{Murrell84}
J.~N. Murrell {\it et~al.}, {\em Molecular Potential Evergy Functions} (John
  Wiley, Chichester, 1984).

\bibitem{Sorbie75}
K.~S. Sorbie and J.~N. Murrell, Molecular Physics {\bf 29},  1387  (1975).

\bibitem{Whitehead76}
R.~J. Whitehead and N.~C. Handy, Journal of Molecular Spectroscopy {\bf 59},
  459  (1976).

\bibitem{Vergiri88}
A. Vegiri and S.~C. Farantos, The Journal of Physical Chemistry {\bf 92},  2723
   (1988).

\bibitem{Carter82}
S. Carter, I.~M. Mills, J.~N. Murrell, and A.~J.~C. Varandas, Molecular Physics
  {\bf 45},  1053  (1982).

\bibitem{Searles93}
D. Searles and E. {von Nagy-Felsobuki}, {\em Ab Initio Variational Calculations
  of Molecular Vibrational-Rotational Spectra} (Springer-Verlag, Berlin, 1993).

\bibitem{Mezey87}
P.~G. Mezey, {\em Potential Energy Hypersurfaces} (Elsevier, Amsterdam, 1987).

\bibitem{Eckart35}
C. Eckart, Physical Review {\bf 47},  552  (1935).

\bibitem{WilsonHoward36}
E.~B. Wilson and J.~B. Howard, The Journal of Chemical Physics {\bf 4},  260
  (1936).

\bibitem{Watson68}
J.~K.~G. Watson, Molecular Physics {\bf 15},  479  (1968).

\bibitem{SutcliffeTennyson87}
B.~T. Sutcliffe and J. Tennyson, Journal of the Chemical Society Faraday
  Transactions 2 {\bf 83},  1663  (1987).

\bibitem{Jensen88}
P. Jensen, Journal of Molecular Spectroscopy {\bf 128},  478  (1988).

\bibitem{LeRoy95}
R.~J. Le{R}oy, Technical report, University of Waterloo (unpublished).

\bibitem{Hutson93}
J.~M. Hutson, {BOUND}: A program for calculating bound-state energies for
  weakly bound molecular complexes, version 1 (1984) to version 5 (1993),
  Computational Project No. 6 of the Science and Engineering Research Council,
  on Heavy Particle Dynamics, 1993, e-mail: J.M.Hutson@durham.ac.uk.

\bibitem{Kinghorn95a}
D.~B. Kinghorn, International Journal of Quantum Chemistry {\bf 57},  141
  (1996).

\bibitem{Kinghorn93}
D.~B. Kinghorn and R.~D. Poshusta, Physical Review A {\bf 47},  3671  (1993).

\bibitem{Kinghorn95b}
D.~B. Kinghorn and R.~D. Poshusta, International Journal of Quantum Chemistry
  (1995), in press.

\bibitem{Poshusta83}
R.~D. Poshusta, International Journal of Quantum Chemistry {\bf XXIV},  65
  (1983).

\bibitem{Biedenharn81}
L.~C. Biedenharn and J.~D. Louck, {\em Angular Momentum in Quantum Physics.
  Theory and Application}, {\em Encyclopedia of Mathematics and Its
  Applications} (Addison-{W}esley, Reading, {MA}, 1981).

\bibitem{Varga95}
K. Varga and Y. Suzuki, Physical Review A {\bf 52},  2885  (1995).

\bibitem{Kozlowski93b}
P. Kozlowski and L. Adamowicz, Physical Review A {\bf 48},  1903  (1993).

\bibitem{Kinghorn96a}
D.~B. Kinghorn and L. Adamowicz, International Journal of Quantum Chemistry
  (1996), in press.

\bibitem{Varga96}
K. Varga and Y. Suzuki, Physical Review C  (1996), in press.

\bibitem{Kolos65}
W. Kolos and L. Wolniewicz, The Journal of Chemical Physics {\bf 43},  2429
  (1965).

\bibitem{Mathematica}
{\em Mathematica}, Wolfram Research, Inc., 100 Trade Center Drive, Champaign,
  Illinois 61820-7237 USA.

\bibitem{Wolniewicz66}
L. Wolniewicz, The Journal of Chemical Physics {\bf 45},  515  (1966).

\bibitem{LeRoy68}
R.~J. Le{R}oy and R.~B. Bernstein, The Journal of Chemical Physics {\bf 49},
  4312  (1968).

\end{thebibliography}

%%%%%%%%%%%%%%%%%%%%%%% End \document\movib2.bbl %%%%%%%%%%%%%%%%%%%%%%
