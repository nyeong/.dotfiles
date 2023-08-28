```bash
# os 의존하는 경우
if [[ $(uname) == "Linux" ]]; then ...; fi

# 쉘 의존하는 경우
if [[ $SHELL == "zsh" ]]; then ...; fi

# 특정 머신에만 필요한 경우...
# 파일을 분리하고 .local에서 불러오는 게 제일 좋지 않나 싶습니다.
if [[ $(hostname) == "uibook" ]]; then ...; fi

# 특정 프로그램에 의존하는 경우
if (( $+commands[command] )); then ...; fi
```
