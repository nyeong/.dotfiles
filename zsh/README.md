# zsh

- `zshrc`: 기본 설정 파일
- `zprofile`: macOS zprofile
- `aliases.zsh`: 편리하게 쓸만한 alias 모음
- `init.zsh`: 각종 프로그램 init
- `p10k.zsh`: p10k 설정 파일

## 환경설정하기

1. `zshrc`를 `~/.zshrc`에 심볼릭 링크

	 ```bash
	 ln -sf ~/.dotfiles/zsh/zshrc ~/.zshrc
	 ```
	
2. `zinit` 설치

  ```bash
  bash -c "$(curl --fail --show-error --silent --location https://raw.githubusercontent.com/zdharma-continuum/zinit/HEAD/scripts/install.sh)"	
  ```
	
## 이렇게 쓰자

왠만한 것들은 `~/.config`나 `~/.local`에 넣어서 홈 디렉토리 깔끔하게 유지하기

프로그램 설치 후 `~/.zshrc`에 붙는 것들은 `init.zsh`로 옮기기