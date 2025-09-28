[version:: 0.1.0]

#   Sintaxe de EXPRESSÕES (termos) sobre $L_1$

Todo termo $t \in L_1$ está em:

$$
\begin{aligned}
\textit{termos} \quad t &::= \\ 
           &\mid \texttt{Integer} \ t  \\
           &\mid \texttt{Boolean} \ t  \\
           &\mid \texttt{OrderedPair} \ t_1,t_2 \\
           &\mid \texttt{Fst} \ t  \\
           &\mid \texttt{Snd} \ t   \\
\end{aligned}
$$

Para todo termo $t \in L_1$, estão definidas as funções totais recursivas:
- `constants(t)`: retorna a lista de constantes usadas no termo $t$
- `depth(t)`: retorna a profundidade do termo $t$
- `size(t)` : retorna o tamanho do termo $t$

---
##  Sintaxe de VALORES em $L_1$
$$
\begin{aligned}
\textit{valores} \quad v &::= \\ 
           &\mid \texttt{Integer} \ n  \\
           &\mid \texttt{Boolean} \ b  \\
           &\mid \texttt{OrderedPair} \ v_1,v_2 \\
\end{aligned}
$$

São valores os termos $t \in L_1$: os *valores numéricos* ($Integer$), os *valores booleanos* ($Boolean$), e os *pares ordenados* ($OrderedPair$).


Para todo termo $t \in L_1$, se $t$ é um valor, então `is_value(`$t$`) = true`. Se um valor $v$ é um *valor numérico* $nv$, então `is_numeric(v) = true`. 


---
##  Sistema de Tipos sobre $L_1$

$$
\begin{aligned}
\textit{tipos} \quad T &::= \\ 
           &\mid \texttt{Int} \\
           &\mid \texttt{Bool} \\
           &\mid \texttt{Pair}\ T_1, T_2 \\
\end{aligned}
$$
