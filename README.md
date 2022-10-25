# .dotfiles

맥북에 사용하고 있는 설정 모음입니다.

- [helix]를 텍스트 에디터로
- [homebrew]를 패키지 매니저로
- [tmux]를 터미널 멀티플렉서로
- [zsh]와 [p10k]로
- [git]을 버전 관리자로
- [hammerspoon]을 자동화 도구로
- [karabiner]를 키 개인화 도구로

사용하고 있습니다.

[helix]: https://helix-editor.com/
[tmux]: https://github.com/tmux/tmux
[zsh]: https://www.zsh.org/
[p10k]: https://github.com/romkatv/powerlevel10k
[git]: https://git-scm.com/
[hammerspoon]: https://www.hammerspoon.org/
[karabiner]: https://karabiner-elements.pqrs.org/
[homebrew]: https://brew.sh/index_ko

## 참고

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
