# 智能系统Lab3 —— 虚拟化学实验室开发文档

## 开发环境介绍

|名称|值|
|:-:|:-:|
|操作系统|Windows 10 / macOS 10.14|
|执行环境|CLIPS 6.30|


## 运行程序

```
(batch "launch.bat")
```

## 项目介绍

该项目利用CLIPS系统，实现一个虚拟的化学实验室，该系统支持催化剂以及外置条件控制（如加热、点燃等），用户可以随时向知识库中加入方程式和物质，若加入的物质以及当前环境符合某个方程式反应的条件，则会自动反应生成生成物。用户可以随时查询实验室中的物质种类以及数量分布情况。



## 界面指南

进入程序后，会出现提示提醒用户输入指令：
```CLIPS
USER> 
```

此时输入help，将获得如下输出，代表程序的运行指南，输入对应的指令执行操作：
```
USER> help
[help] - to view help information
[addm] - to add materials to the laboratory
[printm] - to view current materials in the laboratory
[savem] - to save current material information to material.dat
[loadm] - to load material information from material.dat
[run] - to start reaction
[adde] - to add chemical equation
[env] - to change environment condition
[printenv] - to printout environment information
[halt] - to exit without saving
[saverule] - to save the whole program structure into main.clp
[exit] - to save the equation facts to equation.dat and exit
```

## 使用案例


下面以“双氧水分解”为例，介绍该程序的使用方法：

```
2H2O2 === 2H2O + O2↑， 催化剂 = MnO2， 外置条件 = 无
```


1. 运行程序，输入```adde```指令向知识库中加入方程式：

    ```
    USER> adde
    REACTANTS> "H2O2"
    REACTANT RATIOS> 2
    PRODUCTS> "H2O" "O2"
    PRODUCT RATIOS> 2 1
    CATALYZERS> "MnO2"
    CONDITIONS>
    ```

    + ```REACTANTS>``` 代表反应物
    + ```REACTANT RATIOS>``` 代表反应物在方程式中的系数
    + ```PRODUCTS>``` 生成物
    + ```PRODUCT RATIOS>``` 代表生成物在方程式中的系数
    + ```CATALYZERS>``` 代表催化剂，此处为"MnO2"
    + ```CONDITIONS>``` 代表反应的外置条件，此处为空

2. 输入```printe```可以看到知识库中刚添加的方程式：
    ```
    USER> printe
    ("H2O2") * (2) = ("H2O" "O2") * (2 1) ,catalyzed by ("MnO2") ,with ()
    ```

3. 输入```addm```添加物质，再用```printm```查看已添加的物质：
    ```
    USER> addm
    MATERIAL> "H2O2" 4

    USER> printm
    m(H2O2) = 4 mol
    ```

4. 用户随时可以使用```savem```命令将当前的物质列表保存到```material.dat```文件中：
    ```
    USER> savem
    done
    ```

    ```material.dat```文件内容如图所示：
    ```
    (material (name "H2O2") (amount 4))
    ```

5. 添加催化剂，催化剂在系统中并不是以物质的形式存储，而是作为环境对象的一部分，输入```env```改变环境条件，再输入```printenv```查看环境信息：
    ```
    USER> env
    ACTION> 
    CATALYZERS> "MnO2"

    USER> printenv
    action = ()
    catalyzers = ("MnO2")
    ```

    > 此处的 ```ACTION``` 代表外置条件（如加热等），与创建方程式中的 ```CONDITION``` 相对应，当且仅当方程式的 ```CONDITION``` 属性是环境对象 ```ACTION``` 属性的子集且 ```CATALYZERS``` 是环境对象 ```CATALYZERS``` 属性的子集时，方程式才可能发生反应

6. 输入```run```指令开始反应，再输入```printm```查看反应结果：
    ```
    USER> run

    USER> printm
    m(H2O2) = 0.0 mol
    m(H2O) = 4.0 mol
    m(O2) = 2.0 mol
    ```

7. 系统使用完毕后输入 ```exit``` ，可以回到CLIPS命令行并将当前录入的方程式保存在 ```equation.dat```文件中，并且下次运行程序会自动载入这个文件
    ```
    USER> exit

    CLIPS> 
    ```

    equation.dat 内容如图所示：
    ```
    (equation (name gen3) (reactant-ratio 2) (reactants "H2O2") (product-ratio 2 1) (products "H2O" "O2") (catalyzers "MnO2") (condition) (checking-generation 0))
    ```