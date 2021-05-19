# 项目规划

这是一个用来做文献信息存取、整合、分析、报告和可视化的包。

其主要的的功能包括：

- 能够自动化生成一批文献（如 500 篇 SCI 论文）的通用分析报告；
- 具有丰富、易用的可视化函数；
- 能够在文献的基础上分发、创作，进而成为一个 scidaily 系统；
  （ `biblioreport` 与 `scidaily` 的关系类似于 `treeio` 和 `ggtree`，前者是后者功能的基础）

## 数据源

- **biblio**：Input/Output
  - WoS, Scopus, Pubmed, Google Scholar, ORCID/ResearcherID, ResearchGate, Bibtex, etc
- **analysis**
  - Summarize
    - Count by many fields (PY, ID, SO, etc), Search pattern
    
- **数据库**
  - 示例数据
  - control vacabulary
    - stop word
    - method

##  算法

- 网络分析
  - network analyzer
  - MATLAB 网络实现
  - 网络分析的 membership 添加到数据框中

## 一系列的可视化函数

- text mining 
  - word cloud
  - network
  - tf-idf
  
## 易用性

函数支持 `group_by()` 语法

  ``` {r}
  bib %>% group_by(WC) %>% yearly_growth()
  
  bib %>% group_by(AF) %>% summarize(total = sum(TC, na.rm = TRUE))
  ```
