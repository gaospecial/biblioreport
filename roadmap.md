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
  - 网络分析的 membership 添加到数据框中（这一功能应该添加的 metaTagExtraction 函数中去。
- 简化网络的方式
  - 通过限定网络中节点和边的属性，在建网络时进行筛选
  - 通过对网络聚类后，按聚类生成的 module 进行筛选
- 网络的参数
  - 共现网络的 weight
  - 合作网络的 totalRecord，selfRecord，collaborationRecord
- 判定唯一作者
- 提取可靠的机构信息
- 单词同义词的鉴别（包括单复数、时态、词性、简写形式等的差异）
  - 或许可以通过比较单词
  - `library(qdap)` 可以鉴定同义词

## 一系列的可视化函数

- text mining 
  - word cloud
  - network
  - tf-idf
  
## 易用性

* 函数支持 `group_by()` 语法 [see `tidygraph`]

  ``` {r}
  bib %>% group_by(WC) %>% yearly_growth()
  
  bib %>% group_by(AF) %>% summarize(total = sum(TC, na.rm = TRUE))
  ```

* `filter()` 语法过滤图中的点和边 [see `tidygraph`]

  ```{.r}
  graph %>% filter(degree > 5, type = "node")
  graph %>% filter(pagerank > 0.5, type = "vertex")
  graph %>% filter(weight > 3, type = "edge")
  ```
  
  返回一个过滤后的 `igraph` 对象，如果是网络可以计算获得的参数，实现先计算后返回。
  
  

## 杂项

* 参数名
  - graph 返回 graph 对象
  - plot 返回 ggplot 对象或者绘图
