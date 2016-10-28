import csv
import os
import itertools
from itertools import product

def findIndexes(s, ch):
    return [i for i, letter in enumerate(s) if letter == ch]

def findAllIndexes(string, substr):
    ind = []
    index = 0;
    while(index<len(string)):
        index = string.find(substr, index)
        if index == -1:
            break
        ind.append(index)
        index = index + len(substr)
    return ind

def isAllExist(a, b):
    return all(x in a for x in b)
        
def loadViennaData(filename):
    filep = open(filename)
    csv_reader = csv.reader(filep)
    sgrna = []
    alldata = []
    for row in csv_reader:
        #print row[2]
        sgrna.append(row[1])
        alldata.append(row)
    #sgrna = sgrna[-1]
    sgrna = sgrna[1:]
    #print(sgrna)
    txtfile = open('rnaseq.txt', 'w')
    #writer = csv.writer(txtfile);
    #print(sgrna)
    for row in sgrna:
        txtfile.write("%s\n" % row)
    txtfile.close()
    os.system("RNAfold < rnaseq.txt > output_mfe.txt")
    os.system("RNAheat --Tmin=50 --Tmax=50 < rnaseq.txt > output_heat.txt")
    os.system("rm rnaseq.txt")
    os.system("rm rna.ps")
    
    outfile = open('output_mfe.txt')
    mfestr = []
    for line in outfile:
        mfestr.append(line)
    mfestr = ''.join(mfestr[1:len(mfestr):2])
    outfile.close()
    os.system('rm output_mfe.txt')
    #print(mfestr)
    mfesplit = mfestr.splitlines()
    #print(mfesplit)
    mfenumlist = []
    #mfenumlist.append('MFE')
    for i in range(0, len(mfesplit)):
        strnum = (''.join(mfesplit[i])).split(' ')
        mfenumlist.append(''.join(strnum[len(strnum)-1]).strip('()'))
        #print(mfenumlist)
    mfenumlist = list(map(float, mfenumlist))
    #print(mfenumlist)
    #print(len(alldata[0]))
    alldata[0].insert(len(alldata[0]), 'MFE')
    for i in range(1, len(mfenumlist) + 1):
        alldata[i].insert(len(alldata[i]), mfenumlist[i - 1])
    
    outfile2 = open('output_heat.txt')
    heat = []
    for line in outfile2:
        heat.append(line)
    #heat = ''.join(heat[1:len(heat):2])
    outfile2.close()
    #os.system('rm output_heat.txt')
    #print(heat)
    heatlist = []
    for i in range(0, len(heat)):
        tmp = heat[i].splitlines()
        heatlist.append(tmp[0].rstrip('\n').split(' ')[3])
    heatlist = list(map(float, heatlist))
    #print(heatlist)
        
    alldata[0].insert(len(alldata[0]), 'Heat')
    for i in range(1, len(heatlist) + 1):
        alldata[i].insert(len(alldata[i]), heatlist[i - 1])
    
    file1 = open(filename, 'wb')
    csv_writer = csv.writer(file1)
    csv_writer.writerows(alldata)
    

def checkextrafeatures(sgrnas, feature):
    dlist = []
    if(feature == 'GC_Count'):
        for sgrna in sgrnas:
            dlist.append(len(findIndexes(sgrna,'G')) + len(findIndexes(sgrna,'C')))
    elif(feature == 'AT_Count'):
        for sgrna in sgrnas:
            dlist.append(len(findIndexes(sgrna,'A')) + len(findIndexes(sgrna,'T')))
    elif(feature == 'A_Count'):
        for sgrna in sgrnas:
            dlist.append(len(findIndexes(sgrna,'A')))
    elif(feature == 'C_Count'):
        for sgrna in sgrnas:
            dlist.append(len(findIndexes(sgrna,'C')))
    elif(feature == 'G_Count'):
        for sgrna in sgrnas:
            dlist.append(len(findIndexes(sgrna,'G')))
    elif(feature == 'T_Count'):
        for sgrna in sgrnas:
            dlist.append(len(findIndexes(sgrna,'T')))
    return dlist


def appendtoCSV(dname, datalist, filename):
    filep = open(filename)
    csv_reader = csv.reader(filep)
    alldata = []
    for row in csv_reader:
        alldata.append(row)
    alldata[0].insert(len(alldata[0]), dname)
    for i in range(1, len(datalist) + 1):
        alldata[i].insert(len(alldata[i]), datalist[i - 1])
    file1 = open(filename, 'wb')
    csv_writer = csv.writer(file1)
    csv_writer.writerows(alldata)
    filep.close()
    file1.close()

    
def n_order(sgrnas, substr, pos):
    dlist = []
    for sgrna in sgrnas:
        #print(sgrnas)
        if(pos in findAllIndexes(sgrna, substr)):
            dlist.append(1)
        else:
            dlist.append(0)
    return dlist

if __name__ == "__main__":
    filename = "FC_plus_RES.csv"
    extrafeaturelist = ["GC_Count","AT_Count","A_Count","C_Count","G_Count","T_Count"]
    totalfeature = 0;
    filep = open(filename)
    csv_reader = csv.reader(filep)
    sgrnas = []
    for row in csv_reader:
        sgrnas.append(row[1])
    sgrnas = sgrnas[1:]
    nucleotides = ["A", "C", "G", "T"]
    firstorderfeature = 0;
    ########first order##########
    for nucleotide in nucleotides:
        for i in range(0,30):
            feature = nucleotide + "_" + str(i+1)
            dat = n_order(sgrnas, nucleotide, i)
            appendtoCSV(feature, dat, filename)
            firstorderfeature = firstorderfeature + 1
    print("First Order Features:",firstorderfeature)
    
    ########second order#########
    secondorderfeature = 0;
    secondorder = keywords = [''.join(i) for i in product(nucleotides, repeat = 2)]
    for nucleotide in secondorder:
        for i in range(0,29):
            feature = nucleotide + "_" + str(i+1)
            dat = n_order(sgrnas, nucleotide, i)
            appendtoCSV(feature, dat, filename)
            secondorderfeature = secondorderfeature + 1
    print("Second Order Features:",secondorderfeature)
            
################third order######################
    thirdorderfeature = 0;
    thirdorder = keywords = [''.join(i) for i in product(nucleotides, repeat = 3)]
    for nucleotide in thirdorder:
        for i in range(0,28):
            feature = nucleotide + "_" + str(i+1)
            dat = n_order(sgrnas, nucleotide, i)
            appendtoCSV(feature, dat, filename)
            thirdorderfeature = thirdorderfeature + 1
    print("Third Order Features:",thirdorderfeature)
            
###############fourth order######################
    fourthorderfeature = 0;        
    fourthorder = keywords = [''.join(i) for i in product(nucleotides, repeat = 4)]
    for nucleotide in fourthorder:
        for i in range(0,27):
            feature = nucleotide + "_" + str(i+1)
            dat = n_order(sgrnas, nucleotide, i)
            appendtoCSV(feature, dat, filename)
            fourthorderfeature = fourthorderfeature + 1
    print("Fourth Order Features:",fourthorderfeature)


    for feature in extrafeaturelist:
        dat = checkextrafeatures(sgrnas, feature)
        appendtoCSV(feature, dat, filename)

    loadViennaData(filename)

    #totalfeatures = firstorderfeature + secondorderfeature + thirdorderfeature + fourthorderfeature + 8
    #print('Total Features:',totalfeatures)
