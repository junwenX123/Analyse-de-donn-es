# Analyse-de-donnees
# Détection de Fraude par Carte Bancaire : Analyse Statistique et Modélisation Supervisée

[![Sorbonne Université](https://img.shields.io/badge/Sorbonne%20Universit%C3%A9-Master%202%20Ing%C3%A9nierie%20Math%C3%A9matique-blue)](#)
[![Statut du Projet](https://img.shields.io/badge/Statut-Termin%C3%A9-success)](#)

Ce dépôt contient le code et les résultats de notre projet d'Analyse des Données, réalisé dans le cadre du Master 2 Ingénierie Mathématique  à Sorbonne Université (Promotion 2025/2026). 

##  Contexte et Objectifs

Le projet porte sur la détection de fraude à la carte bancaire à partir de données transactionnelles. La fraude constituant une classe extrêmement rare (1,51 % de l'échantillon, soit 151 fraudes pour 10 000 transactions), la difficulté principale réside dans le fort déséquilibre des classes. 

L'objectif de cette étude est double:
1. **Ranking (Classement)** : Construire un scoring probabiliste capable de maximiser la détection des fraudes et de prioriser les transactions à risque (en privilégiant la métrique PR AUC).
2. **Interprétabilité** : Produire un modèle transparent et cohérent métier, en identifiant statistiquement les facteurs augmentant le risque.

##  Données et Prétraitement

Le jeu de données synthétique (Credit Card Fraud Detection Dataset) comprend 10 000 transactions.
Des contrôles de qualité rigoureux ont été appliqués (absence de valeurs manquantes et de doublons). 

**Feature Engineering clé :**
* `amount` : Transformation robuste en log (`log_amount = log(1 + amount)`) pour traiter la forte asymétrie à droite.
* `transaction_hour` : Création de `hour_group` (6 tranches horaires) pour capter les effets temporels non linéaires.
* Typage adéquat des variables binaires et multi-classes (ex: `merchant_category`, `foreign_transaction`) en facteurs.

##  Méthodologie

### 1. Analyse Statistique (EDA)
Une analyse univariée et bivariée poussée a été menée pour isoler les signaux faibles et forts face à la cible `is_fraud`.
* **Tests pour variables numériques :** Tests de Shapiro-Wilk, Brown-Forsythe, Welch et Wilcoxon.
* **Tests pour variables catégorielles :** Chi-2 ($\chi^2$), exact de Fisher, Cramér V, et Odds Ratio (OR).

### 2. Modélisation Prédictive
Six modèles de **régression logistique** ont été entraînés et comparés sur un split stratifié 80/20:
* **MAIN** : Effets principaux (modèle interprétable de référence).
* **INTER** : Effets principaux + non-linéarités (quadratiques) + interactions métiers ciblées.
* **STEP** : Sélection automatique stepwise (critère AIC) avec un espace incluant les interactions à deux facteurs.
*Chaque structure a été testée en version standard (`unweighted`) et pondérée (`weighted`) pour gérer le déséquilibre.*

##  Résultats et Évaluation

En raison du déséquilibre des classes, les métriques probabilistes et de ranking (PR AUC, Score de Brier, ECE, Calibration) ont été privilégiées par rapport à la simple ROC AUC.

* **Meilleur modèle pour le Scoring/Ranking :** Le modèle `INTER_unw` offre le meilleur compromis avec la PR AUC la plus élevée (0,8243), un excellent score de Brier (0,0052) et une faible erreur de calibration (ECE = 0,004).
* **Meilleur modèle pour la Décision stricte :** Le modèle `STEP_unw` maximise le score F1 (0,7869) au seuil optimal.

**Insights Métier (Facteurs de Risque Principaux) :**
* **Contexte géographique :** Les transactions internationales (`foreign_transaction`) et les incohérences de localisation (`location_mismatch`) sont les marqueurs de risque les plus extrêmes (OR > 3000).
*  **Score de confiance :** Un haut score de confiance de l'appareil (`device_trust_score`) a un fort effet protecteur.
* **Comportement :** Le risque augmente de façon non linéaire avec la vélocité des transactions sur 24h (`velocity_last_24h`).
*  **Temporalité :** Une sur-fraude notable est concentrée la nuit (tranche 00h-03h).

## Outils & Langages
* **Langage :** R (statistiques descriptives, modélisation glm, évaluation)
* **Méthodes statistiques :** Inférence statistique, Régression logistique, Feature Engineering, Stepwise AIC.

##  Structure du Répertoire
* `data/` : Contient le jeu de données transactionnelles (si applicable/public).
* `scripts/` : Scripts d'exploration de données (EDA) et d'entraînement des modèles.
* `docs/` : Rapport de projet complet et présentation PDF.

