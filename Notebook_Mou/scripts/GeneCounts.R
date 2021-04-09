#! /usr/bin/env Rscript

library(tidyverse)
library(magrittr)

# read files
k_bee <- read_delim("~/2021_workshop_transcriptomics/Notebook_Mou/results/bee.genecounts.out.txt", delim="\t")
j_bee <- readxl::read_excel("~/2021_workshop_transcriptomics/Notebook_Jennifer/Bumblebee/results/gsnap_counts.xlsx", sheet="gene")

# look at column names
names(j_bee)
names(k_bee)

k_sub <- k_bee %>%
  select(Geneid, "1-A02-A2_S8_L002_R1_001.fastq") %>%
  pivot_longer(., col="1-A02-A2_S8_L002_R1_001.fastq")

j_sub <- j_bee %>%
  select(Geneid, "1-A02-A2_S8") %>%
  pivot_longer(., col= "1-A02-A2_S8")

test <- rbind(k_sub, j_sub) %>% 
  pivot_wider(., id_cols="Geneid")

# plotting different colors for each source
test %>% subset[1:20,] %>% 
  ggplot(., aes(x=Geneid, y=value), color=name) +
  geom_point()

# plot 1st 10 genes
(test2 <- test[1:10,] %>% 
  pivot_longer(cols = "1-A02-A2_S8_L002_R1_001.fastq":"1-A02-A2_S8") %>% 
  ggplot(., aes(x=Geneid, y=value, color=name)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_color_manual(name="Sample 1-A02-A2_S8 Source",labels=c("Jennifer", "Me"), values=c("darkgreen", "orange")) +
  labs(x= "Gene ID", y= "Gene Count"))

# gene count difference
both2 <- both %>% mutate(gene_count_diff = k_bee-j_bee)
both2[1:10,] %>% 
  ggplot(., aes(x=Geneid, y=gene_count_diff)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = "Gene ID", y = "gene count difference (k-j)")y

# plot Jennifer to mine gene counts - do they overlap or are they very different?
both <- rbind(k_sub, j_sub) %>%
  pivot_wider(., id_cols="Geneid")
names(both) <- c("Geneid", "k_bee", "j_bee")
both %>%
  ggplot(., aes(x=k_bee, y=j_bee), color=) +
  geom_point()


