#!/bin/bash

Rscripts_path=$(echo "/home/manuel.tardaguila/Scripts/R/")

output_dir=$1


Log_files=$(echo "$output_dir""/""Log_files/")

rm -rf $Log_files
mkdir -p $Log_files



#### Collect_files ####

module load R/4.1.0


type=$(echo "1_Collect_files")
outfile_Collect_files=$(echo "$Log_files""outfile_""$type"".log")
touch $outfile_Collect_files
echo -n "" > $outfile_Collect_files
name_Collect_files=$(echo "$type""_job")


Rscript_Collect_files=$(echo "$Rscripts_path""104_FSC_SSC_explorer.R")



path_24h=$(echo "/group/soranzo/manuel.tardaguila/FlowCyt_parameters/20062023/FLOW_stats/24h_stats/")
path_48h=$(echo "/group/soranzo/manuel.tardaguila/FlowCyt_parameters/20062023/FLOW_stats/48h_stats/")
path_72h=$(echo "/group/soranzo/manuel.tardaguila/FlowCyt_parameters/20062023/FLOW_stats/72h_stats/")



myjobid_Collect_files=$(sbatch --job-name=$name_Collect_files --output=$outfile_Collect_files --partition=cpuq --time=24:00:00 --nodes=1 --ntasks-per-node=1 --mem-per-cpu=1024M --parsable --wrap="Rscript $Rscript_Collect_files --path_48h $path_48h --path_24h $path_24h --path_72h $path_72h --type $type --out $output_dir")
myjobid_seff_Collect_files=$(sbatch --dependency=afterany:$myjobid_Collect_files --open-mode=append --output=$outfile_Collect_files --job-name=$(echo "seff""_""$name_Collect_files") --partition=cpuq --time=24:00:00 --nodes=1 --ntasks-per-node=1 --mem-per-cpu=128M --parsable --wrap="seff $myjobid_Collect_files >> $outfile_Collect_files")



### graph_exploration ####

module load R/4.1.0


type=$(echo "2_graph_exploration")
outfile_graph_exploration=$(echo "$Log_files""outfile_""$type"".log")
touch $outfile_graph_exploration
echo -n "" > $outfile_graph_exploration
name_graph_exploration=$(echo "$type""_job")


Rscript_graph_exploration=$(echo "$Rscripts_path""105_FSC_SSC_violin_explorer.R")

 


myjobid_graph_exploration=$(sbatch --dependency=afterany:$myjobid_Collect_files --job-name=$name_graph_exploration --output=$outfile_graph_exploration --partition=cpuq --time=24:00:00 --nodes=1 --ntasks-per-node=1 --mem-per-cpu=1024M --parsable --wrap="Rscript $Rscript_graph_exploration --type $type --out $output_dir")
myjobid_seff_graph_exploration=$(sbatch --dependency=afterany:$myjobid_graph_exploration --open-mode=append --output=$outfile_graph_exploration --job-name=$(echo "seff""_""$name_graph_exploration") --partition=cpuq --time=24:00:00 --nodes=1 --ntasks-per-node=1 --mem-per-cpu=128M --parsable --wrap="seff $myjobid_graph_exploration >> $outfile_graph_exploration")

### stats_exploration ####

module load R/4.1.0


type=$(echo "3_stats_exploration")
outfile_stats_exploration=$(echo "$Log_files""outfile_""$type"".log")
touch $outfile_stats_exploration
echo -n "" > $outfile_stats_exploration
name_stats_exploration=$(echo "$type""_job")


Rscript_stats_exploration=$(echo "$Rscripts_path""106_FSC_SSC_STATS.R")

time_point_selected=$(echo '72_hrs')

#--dependency=afterany:$myjobid_Collect_files

myjobid_stats_exploration=$(sbatch --dependency=afterany:$myjobid_Collect_files  --job-name=$name_stats_exploration --output=$outfile_stats_exploration --partition=cpuq --time=24:00:00 --nodes=1 --ntasks-per-node=1 --mem-per-cpu=1024M --parsable --wrap="Rscript $Rscript_stats_exploration --time_point_selected $time_point_selected --type $type --out $output_dir")
myjobid_seff_stats_exploration=$(sbatch --dependency=afterany:$myjobid_stats_exploration --open-mode=append --output=$outfile_stats_exploration --job-name=$(echo "seff""_""$name_stats_exploration") --partition=cpuq --time=24:00:00 --nodes=1 --ntasks-per-node=1 --mem-per-cpu=128M --parsable --wrap="seff $myjobid_stats_exploration >> $outfile_stats_exploration")

### graphs_2D ####

module load R/4.1.0


type=$(echo "4_graphs_2D")
outfile_graphs_2D=$(echo "$Log_files""outfile_""$type"".log")
touch $outfile_graphs_2D
echo -n "" > $outfile_graphs_2D
name_graphs_2D=$(echo "$type""_job")


Rscript_graphs_2D=$(echo "$Rscripts_path""107_FSC_SSC_density.R")

time_point_selected=$(echo '72_hrs')
subpopulation_selected=$(echo 'Single_pos')

#--dependency=afterany:$myjobid_Collect_files

myjobid_graphs_2D=$(sbatch --dependency=afterany:$myjobid_Collect_files --job-name=$name_graphs_2D --output=$outfile_graphs_2D --partition=cpuq --time=24:00:00 --nodes=1 --ntasks-per-node=1 --mem-per-cpu=1024M --parsable --wrap="Rscript $Rscript_graphs_2D --time_point_selected $time_point_selected --subpopulation_selected $subpopulation_selected --type $type --out $output_dir")
myjobid_seff_graphs_2D=$(sbatch --dependency=afterany:$myjobid_graphs_2D --open-mode=append --output=$outfile_graphs_2D --job-name=$(echo "seff""_""$name_graphs_2D") --partition=cpuq --time=24:00:00 --nodes=1 --ntasks-per-node=1 --mem-per-cpu=128M --parsable --wrap="seff $myjobid_graphs_2D >> $outfile_graphs_2D")









