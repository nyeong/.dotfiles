final: prev: {
  myTexlive = prev.texlive.combine {
    inherit
      (prev.texlive)
      scheme-medium
      kotex-utf
      kotex-utils
      collection-langcjk
      collection-latexextra
      collection-xetex
      xetex
      lualatex-math
      collection-fontsrecommended
      wrapfig
      latexmk
      ;
  };

  myEmacs = (
    let
      myEmacsOverrides = {
        withNativeCompilation = true;
        withSQLite3 = true;
        withTreeSitter = true;
        withWebP = true;
      };

      chosenEmacsBase =
        if prev ? emacs-unstable
        then prev.emacs-unstable
        else prev.emacs;

      chosenEmacs = chosenEmacsBase.override myEmacsOverrides;
    in
      (prev.emacsPackagesFor chosenEmacs).emacsWithPackages (
        epkgs:
          with epkgs; [
            vterm
            treesit-grammars.with-all-grammars
          ]
      )
  );
}
