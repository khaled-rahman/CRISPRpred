s = ['TTTTTTTTTTTTTTTTTTTTTTTTTTTTTT', 'AAAAAAAAAAAAAAAAAAAAAAAAAAAAAA', 'CAGAAAAAAAAACACTGCAACAAGAGGGTA', 'TTTTAAAAAACCTACCGTAAACTCGGGTCA', 'TCAGAAAAAGCAGCGTCAGTGGATTGGCCC', 'AATAAAAAATAGGATTCCCAGCTTTGGAAG']

sgrnaV = []
for term in s:
    sum = 0;
    for i in term:
        if(i == 'A'):
            sum = sum + 1
        elif(i == 'C'):
            sum = sum + 2
        elif(i == 'G'):
            sum = sum + 3
        elif(i == 'T'):
            sum = sum + 4
    sgrnaV.append(sum/120.0)
print(sgrnaV)
