{ powerline-go }:

powerline-go.overrideAttrs (_: {
  src = ./src;
  version = "custom";
  goPackagePath = "github.com/kirens-dotfiles/powerline-go";
})
