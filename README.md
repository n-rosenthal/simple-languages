"""
status: 'on-going'
last-updated: '2026-11-13'
version: '1.0.1'
"""

está é uma versão inicial da documentação do projeto `simple-languages`.

# `simple-languages`
`simple-languages` é um projeto de pesquisa e estudos em linguagens de programação, teoria dos tipos,  semântica e especificação formais. baseando-se inicialmente no [Types and Programming Languages](https://www.cis.upenn.edu/~bcpierce/tapl/) (Pierce /et al/, 1998), tentamos implementar uma base comum à diversas linguagens distintas; a partir desta base, implementamos as linguagens expostas no livro, bem como outros conceitos. por fim, as linguagens são compiladas para uma linguagem de máquina abstrata (sobre uma máquina de pilha).

todas as linguagens são expostas em um /front-end/ em WASM e podem ser testadas online (é necessário, no entanto, no presente momento, servir você mesmo o servidor local); as implementações contam com interpretadores e compiladores próprios, além de demonstrações da sintaxe, do sistema de tipos e da avaliação.

enquanto projeto de estudos, este repositório também busca armazenar o aprendizado do autor sobre o TAPL, bem como de outras fontes; a ideia é que o conhecimento seja partilhado com outros desenvolvedores e que sirva de base para as implementações.