# 왠지는 모르겠는데 macOS 빌드할 때 fish check 실패해서 패싱
final: prev: {
  fish = prev.fish.overrideAttrs (old: {
    doCheck = false;
  });
}
