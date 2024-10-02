{
  pkgs,
  ...
}: {
  wrappers.git = {
    basePackage = pkgs.gitFull;
    env.GIT_CONFIG_GLOBAL.value = ../../conf/gitconfig;
  };
}
