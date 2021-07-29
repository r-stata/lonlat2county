# lonlat2county: 根据经纬度判断所处的省市区县

> 在线：https://czxb.shinyapps.io/lonlat2county/   
> 本地：shiny::runGitHub("r-stata/lonlat2county")  
> 下载之后在本地：shiny::runApp("lonlat2county")  

---

最近一个小伙伴给我发了一个数据集，是 2001 年到 2018 年所有上市公司的经纬度数据。她想知道每个公司所处的省份，我就帮她计算了一下。之前我讲解过使用 R 语言的解决办法，但是她表示不会 R 语言，所以这次课程的结尾也有 Stata 的解决办法。

另外除了本文讲解的方法，还可以使用调用高德地图地理逆编码接口的方法，大家感兴趣的话可以自行尝试，可以参考之前的调用高德地图地理编码接口的方法。

## R 语言：sf 包

首先我们先把加载相关的 R 包、设定绘图主题和读取一份中国省份地图数据，读入之后将坐标系转换为 laea 坐标系：

> 注意代码里面的 cnfont 是我在 Profile 里面配置的代码，如果没有配置可以删除相关参数或者根据[「R 数据科学」系列课程](https://rstata.duanshu.com/#/course/d05f10013b3d4d1587d1d2febe6031d5)的第一课进行配置。

```r
library(sf)
library(tidyverse)
library(ggplot2)
library(readxl)
library(ggtext)
library(ggspatial)

# 设定绘图主题
source("theme.R")

# 由于在地理计算中各个对象的坐标参考系需要保持一致，所以统一使用下面的坐标参考系 
crs <- "+proj=laea +lat_0=23 +lon_0=113 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"

# 读取一份中国省级地图数据
cnmap <- read_sf('2019行政区划SHP/省.shp') %>% 
  st_transform(crs = crs) %>% 
  st_make_valid() %>% 
  st_simplify(dTolerance = 0.02)

# 查看 cnmap 的 crs
st_crs(cnmap)

#> Coordinate Reference System:
#>   User input: +proj=laea +lat_0=23 +lon_0=113 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs 
#>   wkt:
#> PROJCRS["unknown",
#>     BASEGEOGCRS["unknown",
#>         DATUM["Unknown based on GRS80 ellipsoid",
#> ......
#>                 ID["EPSG",9001]]]]

st_crs(cnmap)$proj4string 

#> [1] "+proj=laea +lat_0=23 +lon_0=113 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs"
```

`theme.R` 的文件内容如下：

```r
theme_set(
  theme_minimal(base_family = cnfont) + 
    theme(
      plot.background = element_rect(fill = "white", 
                                     color = "grey50", 
                                     size = 3),
      panel.background = element_rect(fill = "#cbe6f0", color = "#cbe6f0"),
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      legend.text = element_markdown(size = 13, 
                                     color = "grey50"),
      plot.title = element_text(size = 24, 
                                color = "grey50",
                                face = "bold",
                                hjust = 0.5,
                                margin = margin(0, 0, 10, 0)),
      plot.subtitle = element_text(size = 12, 
                                   color = "grey50",
                                   face = "bold",
                                   hjust = 0.5,
                                   margin = margin(0, 0, 10, 0)),
      plot.caption = element_text(size = 10, 
                                  color = "grey50",
                                  hjust = 0.5),
      plot.margin = margin(30, 30, 30, 30),
      panel.grid = element_line(color = "grey95")
    )
)
```

最新版本的 sf 包在打印 crs 的时候采用上面的形式，不过对 crs 的设置依然支持 proj4strings（也就是这个：`+proj=laea +lat_0=23 +lon_0=113 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs`）。

再读取待处理的上市公司经纬度数据以及将这个数据转换成和 cnmap 同样 crs 的 sf 对象：

> st_transform() 是用来进行投影转换的，进行地理计算的时候，各对象的 crs 要保持一致。

```r
# 读取九段线的数据
read_sf("九段线.geojson") -> jdx

# 读取待处理的数据以及将这个数据转换成和 cnmap 同样 crs 的 sf 对象
read_xlsx('geo.xlsx') %>% 
  dplyr::filter(!is.na(lng), !is.na(lat)) %>% 
  st_as_sf(coords = c("lng", "lat"), crs = 4326) %>% 
  st_make_valid() -> df
df

#> Simple feature collection with 39166 features and 2 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 18.32654 ymin: 18.21693 xmax: 132.9816 ymax: 132.9816
#> Geodetic CRS:  WGS 84
#> # A tibble: 39,166 x 3
#>     code  year            geometry
#>  * <dbl> <dbl>         <POINT [°]>
#>  1     1  2013 (114.1077 22.54068)
#>  2     1  2014 (114.1077 22.54068)
#>  3     1  2015 (114.1077 22.54068)
#>  4     1  2000  (114.1157 22.5424)
#>  5     1  2003 (114.1253 22.53967)
#>  6     1  2004 (114.1077 22.54068)
#>  7     1  2005 (114.1077 22.54068)
#>  8     1  2006 (114.1077 22.54068)
#>  9     1  2007 (114.1077 22.54068)
#> 10     1  2008 (114.1077 22.54068)
#> # … with 39,156 more rows

df %>% 
  st_transform(crs) -> df
```

然后就可以进行省份归类啦，需要注意，进行地理计算的时候，各个 sf 对象应该是同一 crs 的：

```r
df %>% 
  st_intersection(cnmap) -> df
df

#> Simple feature collection with 37616 features and 5 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 1207495 ymin: 2685282 xmax: 5890910 ymax: 6124680
#> CRS:           +proj=laea +lat_0=23 +lon_0=113 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs
#> # A tibble: 37,616 x 6
#>     code  year 省代码 省     类型            geometry
#>  * <dbl> <dbl>  <dbl> <chr>  <chr>        <POINT [m]>
#>  1     8  2015 110000 北京市 直辖市 (4610367 5085602)
#>  2     8  2016 110000 北京市 直辖市 (4610367 5085602)
#>  3     8  2017 110000 北京市 直辖市 (4610367 5085602)
#>  4    10  2010 110000 北京市 直辖市 (4577116 5139321)
#>  5    10  2011 110000 北京市 直辖市 (4577116 5139321)
#>  6    10  2013 110000 北京市 直辖市 (4610763 5084248)
#>  7    10  2012 110000 北京市 直辖市 (4610763 5084248)
#>  8    10  2015 110000 北京市 直辖市 (4610763 5084248)
#>  9    10  2014 110000 北京市 直辖市 (4610763 5084248)
#> 10    18  2015 110000 北京市 直辖市 (4625515 5068770)
#> # … with 37,606 more rows
```

绘图展示：

```r
ggplot() + 
  geom_sf(data = cnmap, size = 0.2, fill = "gray90") + 
  geom_sf(data = jdx, size = 0.4) + 
  geom_sf(data = subset(df, year == 2018), aes(color = 省),
          shape = 15, alpha = 0.8, size = 3) + 
  scale_color_manual(values = rep(c("#D7BA9F", "#800080", "#1C3181",  "#1BB6AF",  "#FFAD0A",  "#EE6100",  "#D72000"), 5)) + 
  labs(title = "中国上市公司的分布",
       caption = "绘制：微信公众号 RStata",
       color = "",
       subtitle = "根据上市公司的经纬度判断所处的省份，然后绘图展示。") + 
  theme(legend.position = "right") + 
  guides(color = guide_legend(nrow = 28)) + 
  annotation_scale(
    width_hint = 0.2,
    text_family = cnfont,
    pad_x = unit(0.5, "cm")
  ) + 
  annotation_north_arrow(
    location = "tr", which_north = "false",
    width = unit(1.6, "cm"), 
    height = unit(2, "cm"),
    style = north_arrow_fancy_orienteering(
      text_family = cnfont
    )
  ) -> p 
ggsave("pic1.png", plot = p, width = 18, height = 9) 
knitr::plot_crop('pic1.png') 
```

![](https://mdniceczx.oss-cn-beijing.aliyuncs.com/image_20210728142347.png)

是不是觉得绘图区域有点窄？可以使用下面的方法调整：

```r
# 查看 cnmap 的 bbox
st_bbox(cnmap)
```

然后就可以根据 xmin 和 xmax 设定绘图区域了：

```r
p + 
  coord_sf(xlim = c(985595.1, 5976026.9) + c(-80, 80) * 10000) -> p1
ggsave("pic2.png", plot = p1, width = 18, height = 9) 
knitr::plot_crop('pic2.png') 
```

![](https://mdniceczx.oss-cn-beijing.aliyuncs.com/image_20210728142400.png)

然后选择我们需要的几个变量保存即可：

```r
# 保存数据
df %>% 
  select(code, year, 省, 省代码) %>% 
  st_drop_geometry() %>% 
  writexl::write_xlsx("省份归类.xlsx")
```

再例如深圳各个区的：

```r
# 再例如深圳各个区的
read_sf("2019行政区划SHP/县.shp") %>% 
  subset(市 == "深圳市") %>% 
  st_transform(crs) -> sz

# 提取出深圳市的上市公司
df %>% 
  st_intersection(sz) -> szdf

# 绘图展示
ggplot() + 
  geom_sf(data = sz, size = 0.2, fill = "gray90") + 
  geom_sf(data = subset(szdf, year == 2018), 
          aes(color = NAME),
          shape = 15, alpha = 0.8, size = 3) + 
  scale_color_manual(values = c("#fed439", "#709ae1", "#8a9197", "#d2af81", "#fd7446", "#d5e4a2", "#197ec0", "#f05c3b", "#46732e")) + 
  labs(title = "深圳上市公司的分布",
       caption = "绘制：微信公众号 RStata",
       color = "",
       subtitle = "根据上市公司的经纬度判断所处的省份，然后绘图展示。") + 
  theme(legend.position = "right") + 
  guides(color = guide_legend(nrow = 28)) + 
  annotation_scale(
    width_hint = 0.2,
    text_family = cnfont,
    pad_x = unit(0.5, "cm")
  ) + 
  annotation_north_arrow(
    location = "tr", which_north = "false",
    width = unit(1.6, "cm"), 
    height = unit(2, "cm"),
    style = north_arrow_fancy_orienteering(
      text_family = cnfont
    )
  ) -> p2
ggsave("pic3.png", plot = p2, width = 16, height = 9) 
knitr::plot_crop('pic3.png') 
```

![](https://mdniceczx.oss-cn-beijing.aliyuncs.com/image_20210728142433.png)

然后就可以选择想要的变量保存了：

```r
# 保存
szdf %>% 
  select(code, year, 县代码 = PAC, 县 = NAME) %>% 
  mutate(city = "深圳") %>% 
  st_drop_geometry() %>% 
  writexl::write_xlsx("深圳的上市公司.xlsx")
```

## R 语言：使用我们编写的一个小 Shiny 应用

为了方便大家把经纬度解析成省市区县，我们编写了一个 Shiny 应用，可以使用下面两种方式运行：

1. 在线：https://czxb.shinyapps.io/lonlat2county/ 
2. 本地：shiny::runApp("lonlat2county")

为了让这个 shiny 应用足够简单，所以没有设计太多复杂的功能，就是上传一个 csv 文件、运行、下载结果数据（复杂的功能往往会导致运行很慢）。

首先准备 csv 文件：

```r
# 准备数据
# 我们需要准备一个 csv 文件，这个文件应该包含数值型的 lon lat 变量，不能有缺失值：
read_xlsx('geo.xlsx') %>% 
  dplyr::filter(!is.na(lng), !is.na(lat)) %>% 
  rename(lon = lng) %>% 
  slice(1:1000) %>% 
  write_csv("temp.csv")
```

然后运行 shiny app：

```r
shiny::runApp("lonlat2county")
```

选择刚刚保存的 "temp.csv" 文件然后耐心等待界面右侧出现预览结果即可下载（可能要等数分钟）

![](https://mdniceczx.oss-cn-beijing.aliyuncs.com/image_20210728142442.png)

如果使用 https://czxb.shinyapps.io/lonlat2county/ 网站进行解析，建议每次不要解析超过 1 万个观测值，否则网站可能会崩溃。

结果如下：

```r
read_csv("结果数据.csv") 

#> # A tibble: 962 x 11
#>      lon   lat  code  year 县代码 县     省代码 省     市代码 市     类型  
#>    <dbl> <dbl> <dbl> <dbl>  <dbl> <chr>   <dbl> <chr>   <dbl> <chr>  <chr> 
#>  1  116.  39.9    46  2010 110101 东城区 110000 北京市 110000 北京市 市辖区
#>  2  116.  39.9    46  2011 110101 东城区 110000 北京市 110000 北京市 市辖区
#>  3  116.  39.9    46  2013 110101 东城区 110000 北京市 110000 北京市 市辖区
#>  4  116.  39.9    46  2012 110101 东城区 110000 北京市 110000 北京市 市辖区
#>  5  116.  39.9    46  2014 110101 东城区 110000 北京市 110000 北京市 市辖区
#>  6  116.  39.9    46  2015 110101 东城区 110000 北京市 110000 北京市 市辖区
#>  7  116.  39.9    46  2016 110101 东城区 110000 北京市 110000 北京市 市辖区
#>  8  116.  39.9    46  2017 110101 东城区 110000 北京市 110000 北京市 市辖区
#>  9  116.  39.9    46  2018 110101 东城区 110000 北京市 110000 北京市 市辖区
#> 10  116.  39.9     8  2015 110102 西城区 110000 北京市 110000 北京市 市辖区
#> # … with 952 more rows
```

## Stata：如何使用 lonlat2county 应用？

首先对数据进行初步的处理：

```stata
clear all
cd "~/Desktop/如何根据经纬度判断该地点所处的省份？"
import excel using "geo.xlsx", clear first
ren lng lon
drop if missing(lon) | missing(lat)
* 使用前 1000 个演示
keep in 1/1000
export delimited using "temp2.csv", replace 
```

打开网站：https://czxb.shinyapps.io/lonlat2county/ 

> 网站打开较慢，耐心等待即可。

上传刚刚保存的 temp2.csv 文件，然后耐心等待界面右侧出现预览结果即可下载（可能要等数分钟）。建议每次不要解析超过 1 万个观测值，否则网站可能会崩溃。

![](https://mdniceczx.oss-cn-beijing.aliyuncs.com/image_20210728142442.png)

查看结果数据:

```stata
import delimited using "结果数据.csv", clear encoding(utf8)
```

![](https://mdniceczx.oss-cn-beijing.aliyuncs.com/image_20210728142452.png)

也就是把解析的过程用这个 Shiny 应用完成，其它的都可以用 Stata 处理。

