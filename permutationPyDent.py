import numpy as np
import pandas as pd
import itertools
from tqdm import tqdm


reSeed = 1
np.random.seed(reSeed)
psychTraits = ["phqScore","burnScore","gadScore"] # characteristics of interest
gradesALL = ["D1","D2","D3","D4"] # all grades
gradesBTWX = ["D1","D3"] # grades for between years
resamplesCt = [1000, 1_000_000][1]
alpha = 0.05

mhData = pd.read_csv("C:\\Users\\trogi\\Downloads\\consultDentistry\\mhDataFINALGITHUB.csv")
secondSampleOnly = pd.read_csv("C:\\Users\\trogi\\Downloads\\consultDentistry\\secondSampleOnlyFINALGITHUB.csv")


results = []

def welchTStat(this,minusThis):
    xb1, xb2 = np.mean(this), np.mean(minusThis)
    var1, var2 = np.var(this, ddof=1),np.var(minusThis, ddof=1)
    n1, n2 = len(this), len(minusThis)
    se = np.sqrt(var1/n1 + var2/n2)
    out = (xb1-xb2)/se
    return out


def studentizedPermutation(this,minusThis, resamplesCt, loadingLabel = "permutation t test"):
    comboData = np.concatenate([this,minusThis]) # make data one dimensional
    labels = np.array( [0]*len(this) +  [1]*len(minusThis)) # make ones and zeros to denote group
    observedt = np.abs( welchTStat(this,minusThis) )
    permStats = []
    for _ in tqdm(range(resamplesCt), desc=loadingLabel, leave=False ):
        permutedLabels = np.random.permutation(labels)
        permThis = comboData[permutedLabels == 0]
        permMinusThis = comboData[permutedLabels == 1]
        tStat = np.abs( welchTStat(permThis,permMinusThis) )
        permStats.append(tStat)
    pVal = np.mean(np.array(permStats) >= observedt )
    return pVal, observedt

def welchAnovaF(data, groups):
    groupLabels = np.unique(groups)
    ni = np.array( [ np.sum(groups == g) for g in groupLabels ] )
    xbi = np.array( [ np.mean(data[groups == g]) for g in groupLabels ] )
    vari = np.array( [ np.var(data[groups == g], ddof=1) for g in groupLabels ] )
    wi = ni / vari
    w = np.sum(wi)
    xbDash = np.sum(wi*xbi) / w
    k = len(groupLabels)

    numerator = (np.sum( wi*(xbi-xbDash)**2 ) ) / (k-1)
    denominator = 1 + ( 2*(k-2)/(k**2-1) ) * np.sum( (1-wi/w)**2 / (ni-1) ) 
    F = numerator / denominator
    return F

def welchPermutationANOVA(df, trait, groupCol, groupOrder, resamplesCt, desc = "" ):
    allData = []
    allLabels = []
    for group in groupOrder:
        data = df[df[groupCol] == group][trait].dropna().values
        allData.append(data)
        allLabels.extend( [group]*len(data) )
    combinedData = np.concatenate(allData)
    combinedLabels = np.array(allLabels)
    observedF = welchAnovaF(combinedData, combinedLabels)
    permStatsF = []
    for _ in tqdm(range(resamplesCt), desc=f"{desc} ({trait})", leave=False ):
        permutedLabels = np.random.permutation(combinedLabels)
        F = welchAnovaF(combinedData, permutedLabels)
        permStatsF.append(F)
    pVal = np.mean(np.array(permStatsF) >= observedF )

    return {
        "psychTrait": trait,
        "observedF": observedF,
        "permutationP": pVal
    }
    


# BETWEEN YEARS

for trait in tqdm(psychTraits, desc="BETWEEN YEAR TRAITS"):
    for grade in tqdm(gradesBTWX, desc=f"Grade {trait}", leave=False):
        group2024 = mhData[(mhData['DENTALYR_NESTED'] == f"{grade}.2024")][trait].dropna().values
        group2025 = mhData[(mhData['DENTALYR_NESTED'] == f"{grade}.2025")][trait].dropna().values

        pVal, obsStat = studentizedPermutation(
            group2024,group2025, resamplesCt,
            loadingLabel=f"Perm {grade} 2024-2025 ({trait})"
        )

        results.append({
            'typeSci': 'ACROSS YEARS',
            'psychTrait': trait,
            'this': f"{grade}.2024",
            'minusThis': f"{grade}.2025",
            'Observed_Stat': obsStat,
            'rawP': pVal
        })

# WITHIN YEARS
gradePairs = list(itertools.combinations(gradesALL,2))

for trait in tqdm(psychTraits, desc = "WITHIN YEAR TRAITS"):
    for this, minusThis in tqdm(gradePairs, desc=f"Pair {trait}", leave=False):
        thisGroup = secondSampleOnly[(secondSampleOnly['DENTALYR_NESTED'] == f"{this}.2025")][trait].dropna().values
        minusThisGroup = secondSampleOnly[(secondSampleOnly['DENTALYR_NESTED'] == f"{minusThis}.2025")][trait].dropna().values

        pVal, obsStat = studentizedPermutation(
            thisGroup, minusThisGroup, resamplesCt,
            loadingLabel=f"Perm {this}-{minusThis} ({trait})" 
        )
        
        results.append({
            'typeSci': 'WITHIN YEAR',
            'psychTrait': trait,
            'this': f"{this}.2025",
            'minusThis': f"{minusThis}.2025",
            'Observed_Stat': obsStat,
            'rawP': pVal           
        })

resultsDf = pd.DataFrame(results)
resultsDf["adjP"] = resultsDf["rawP"]*6
resultsDf["adjPRESULT"] = resultsDf["adjP"] <= alpha

print("\nFinal Pairwise Studentized Permutation Results:")
print(resultsDf)


# OVERAL GLOBALS

globResults = []
for trait in tqdm(psychTraits, desc =  "GLOBAL ANOVA TRAITS"):
    result = welchPermutationANOVA(
        secondSampleOnly,
        trait=trait,
        groupCol="DENTALYR_NESTED",
        groupOrder=[f"{grad}.2025" for grad in gradesALL],
        resamplesCt=resamplesCt,
        desc="PERM ANOVA"
    )
    globResults.append(result)

globDf = pd.DataFrame(globResults)
print("\nOverall Welch ANOVA Permutation Results (2025):")
print(globDf)