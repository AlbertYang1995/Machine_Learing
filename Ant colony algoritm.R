antnum = 4      #蚂蚁数目
citysum = 6     #城市数目     
iter = 5        #迭代次数
iter.counter = 0
city.counter = 0
city.temp = 0         #目前的城市
city.next = 0         #下一个城市
city.anext = 0        #下下个城市
citysign <- matrix(0, nrow = 1, ncol = 6)      #标记走过的城市
infsum = 0            #信息素总和
infrel = 1.2          #每只蚂蚁释放信息素的量
infleft = 0.5         #信息素蒸发比例
infran = 0            #随机选取一个信息素的值
infsle = 0            #选取一个真实的信息素的值
dissum = 0            #总距离
perfdissum = 999      #最短距离
i = 0
distance <- c(0, 3, 1, 5, 4, 3, 1, 0, 5, 4, 3, 3, 5, 4, 0, 2, 1, 3, 3, 1, 3, 0,
              3, 3, 5, 2, 4, 1, 0, 3, 2, 2, 2, 2, 2, 0)
names <- c(1, 2, 3, 4, 5, 6)
disbtw <- matrix(distance, nrow = 6, ncol = 6, byrow = TRUE, dimnames = list(names, names))
infbtw <- matrix(0, nrow = 6, ncol = 6, dimnames = list(names, names))    #设置空矩阵放置信息素

if (iter > 0) {
  
  #-----------------------------初始蚂蚁寻找路径----------------------------------
  if (antnum == 4) {
    
    if (sum(citysign) == 0) {
      city.temp = sample(1:6, size = 1)  #轮盘赌
      citysign[][city.temp] = 1
    }
    if (infsum == 0) {
      city.next = round(runif(1, 1, 6))  #轮盘赌
      if (citysign[][city.next] == 0) {
        city.next = round(runif(1, 1, 6))  #轮盘赌
      }
      citysign[1][city.next] = 1
    }
    
    #----------------统计距离和信息素--------------------
    dissum = disbtw[city.temp][city.next] + dissum
    infbtw[city.temp][city.next] = 1 / (disbtw[city.temp][city.next] * infrel)
    infsum = infbtw[city.temp][city.next] + infsum
    
    while(sum(citysign) != 6) {
      city.anext = round(runif(1, 1, 6))  #轮盘赌
      while (city.anext == city.temp | city.anext == city.next) 
        city.anext = round(runif(1, 1, 6))  #轮盘赌
      
      city.temp = city.next
      city.next = city.anext
      citysign[][city.next] = 1
      
      dissum = disbtw[city.temp][city.next] + dissum
      infbtw[city.temp][city.next] = infbtw[city.temp][city.next] + 1 / (disbtw[city.temp][city.next] * infrel)
      infsum = infbtw[city.temp][city.next] + infsum
    }
    dissum
    antnum = antnum - 1	
    citysign = 0
    dissum = 0
  }   
  #--------------信息素蒸发--------------
  infbtw = infbtw * infleft
  
  #---------------------第二只蚂蚁出动---------------------
  if (antnum > 0) {
    city.temp = sample(1:6, size = 1)  #随机选择落点
    citysign[][city.temp] = 1
    
    infran = runif(0, infsum)
    infsle = sample(infbtw[city.temp][], size = 1)
    if (infran - infsle < 0) {
      for (i in 1:6) {
        if (infbtw[city.temp][i] == infsle)
          city.next = i
      }
    }
    citysign[][city.next] = 1
    dissum = disbtw[city.temp][city.next] + dissum
    infbtw[city.temp][city.next] = infbtw[city.temp][city.next] + 1 / (disbtw[city.temp][city.next] * infrel)
    
    #--------------更新信息素----------------
    #if (infbtw[city.temp][city.next] = 0)
    #  infbtw[city.temp][city.next] = 1 / (disbtw[city.temp][city.next] * infrel)
    #else
    #  infbtw[city.temp][city.next] = infbtw[city.temp][city.next] + 1 / (disbtw[city.temp][city.next] * infrel)
    if (sum(citysign) != 6) {
      infsum = sum(infbtw)
      infran = runif(0, infsum)
      infsle = sample(infbtw[city.next][], size = 1)
      if (infran - infsle < 0) {
        for (i in 1:6) {
          if (infbtw[city.next][i] == infsle)
            city.anext = i
        } 
      }
      city.temp = city.next
      city.next = city.anext
      citysign[][city.next] = 1
      dissum = disbtw[city.temp][city.next] + dissum
      infbtw[city.temp][city.next] = infbtw[city.temp][city.next] + 1 / (disbtw[city.temp][city.next] * infrel)
    }
    if (perfdissum > dissum)
      perfdissum = dissum
    
    antnum = antnum - 1	
    citysign = 0
    dissum = 0
    
  }
  
  iter = iter - 1
  
}  

cat("最短路径为", perfdissum)

















































