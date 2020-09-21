#Bing Wallpaper UHD for Windows
import requests
import re

url = 'https://cn.bing.com/'
html = requests.get(url).text
matchimglnk = re.findall('th.*=OHR.*UHD.jpg',html)

filena = re.findall(r'\w+_ZH.*\d+_UHD.jpg',html)
for temp in filena:
    filename = temp

for lnk in matchimglnk:
    url = 'https://cn.bing.com/' + lnk
    print(url)
    pic = requests.get(url)
    picpath = r'C:\Users\CHANH\Downloads\%s' % (filename)
    with open(picpath,'wb') as f:
        f.write(pic.content)
    f.close()
    print ('Successfully downloaded in ',picpath)
    #open(r'C:\Users\CHANH\Downloads\%s' %(filename),'wb').write(pic.content)