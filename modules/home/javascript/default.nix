{pkgs, ...}: {
  home.packages = with pkgs; [
    nodejs
    bun
    pnpm
    yarn
    deno

    typescript
    typescript-language-server

    biome
  ];
}
