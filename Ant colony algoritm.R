antnum = 4      #������Ŀ
citysum = 6     #������Ŀ     
iter = 5        #��������
iter.counter = 0
city.counter = 0
city.temp = 0         #Ŀǰ�ĳ���
city.next = 0         #��һ������
city.anext = 0        #���¸�����
citysign <- matrix(0, nrow = 1, ncol = 6)      #����߹��ĳ���
infsum = 0            #��Ϣ���ܺ�
infrel = 1.2          #ÿֻ�����ͷ���Ϣ�ص���
infleft = 0.5         #��Ϣ����������
infran = 0            #���ѡȡһ����Ϣ�ص�ֵ
infsle = 0            #ѡȡһ����ʵ����Ϣ�ص�ֵ
dissum = 0            #�ܾ���
perfdissum = 999      #��̾���
i = 0
distance <- c(0, 3, 1, 5, 4, 3, 1, 0, 5, 4, 3, 3, 5, 4, 0, 2, 1, 3, 3, 1, 3, 0,
              3, 3, 5, 2, 4, 1, 0, 3, 2, 2, 2, 2, 2, 0)
names <- c(1, 2, 3, 4, 5, 6)
disbtw <- matrix(distance, nrow = 6, ncol = 6, byrow = TRUE, dimnames = list(names, names))
infbtw <- matrix(0, nrow = 6, ncol = 6, dimnames = list(names, names))    #���ÿվ��������Ϣ��

if (iter > 0) {
  
  #-----------------------------��ʼ����Ѱ��·��----------------------------------
  if (antnum == 4) {
    
    if (sum(citysign) == 0) {
      city.temp = sample(1:6, size = 1)  #���̶�
      citysign[][city.temp] = 1
    }
    if (infsum == 0) {
      city.next = round(runif(1, 1, 6))  #���̶�
      if (citysign[][city.next] == 0) {
        city.next = round(runif(1, 1, 6))  #���̶�
      }
      citysign[1][city.next] = 1
    }
    
    #----------------ͳ�ƾ������Ϣ��--------------------
    dissum = disbtw[city.temp][city.next] + dissum
    infbtw[city.temp][city.next] = 1 / (disbtw[city.temp][city.next] * infrel)
    infsum = infbtw[city.temp][city.next] + infsum
    
    while(sum(citysign) != 6) {
      city.anext = round(runif(1, 1, 6))  #���̶�
      while (city.anext == city.temp | city.anext == city.next) 
        city.anext = round(runif(1, 1, 6))  #���̶�
      
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
  #--------------��Ϣ������--------------
  infbtw = infbtw * infleft
  
  #---------------------�ڶ�ֻ���ϳ���---------------------
  if (antnum > 0) {
    city.temp = sample(1:6, size = 1)  #���ѡ�����
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
    
    #--------------������Ϣ��----------------
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

cat("���·��Ϊ", perfdissum)
















































