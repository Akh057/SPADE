import scala.io.StdIn._
import scala.math
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer
import scala.io.Source
import java.io.PrintWriter
import util.control.Breaks._

object Spade
{
	// The buffer that stores the strings in the dataset
	var DB = ArrayBuffer[String]();
	
	// The frequent sequences of the dataset are stored here
	var Frequent_Sequences = ListBuffer[String]();
	var Maximal_Frequent_Sequences = ListBuffer[String]();

	var FrequentSeq_SupportCount = collection.mutable.Map[String,Int]();

	// We extract the Strings from the file and store in the global dataset
	def extract(Filename:String):Unit={
		for(line<-Source.fromFile(Filename,"UTF-8").getLines()){
			DB += line;
		}		
	}

	// This variable holds the vertical tid database i.e. each symbol or sequences has a list of tuples of the form <tid,poslist>
	var vertical_DB_1 = collection.mutable.Map[String, collection.mutable.Map[Int,List[Int]]]();
	var index = 0;

	// This method is to build the vertical database from the given dataset 
	def build():Unit={
		// iterating over the entire array of strings
		for(i<-0 to DB.length-1){
			// for each symbol we build the vertical tid database i.e. the map of maps of lists 
			for(j<-0 to DB(i).length - 1){
				
				//  for each symbol see if its present in the tid list map if yes look for the transaction if yes then add the index of the symbol to the list 
				if(vertical_DB_1.contains(DB(i)(j).toString)){
					if(vertical_DB_1(DB(i)(j).toString).contains(i)){
						(vertical_DB_1(DB(i)(j).toString))(i) ++= List(j);
						// printf("0 Debug: %d %d\n",i,j);
					}
					// if the symbol doesn't have the transaction
					else{
						var temp_1 = List[Int]();
						temp_1 ++= List(j);
						vertical_DB_1(DB(i)(j).toString)(i) = temp_1;
					}
				}
				// if the tidlist doesn't have the symbol 
				else{
					var temp = collection.mutable.Map[Int,List[Int]]();
					var temp_1 = List[Int]();
					temp_1 ++= List(j);
					temp(i) = temp_1;
					vertical_DB_1(DB(i)(j).toString) = temp;
				}
			}
		}
	}


	def preProcess(Path:String):Unit = {
		this.extract(Path);
		this.build();
	}


	// Check if we can merge the two subsequences

	def shareSubseq(seq1:String,seq2:String):Boolean = {
		if(seq1.length != seq2.length){
			false
		}

		if(seq2.startsWith(seq1.substring(0,seq1.length-1))){
			true
		}

		else{
			false
		}
	}

	// This is to obtain the intersection of the two equivalence classes

	def Intersection_Lists(seq1:collection.mutable.Map[Int,List[Int]],seq2:collection.mutable.Map[Int,List[Int]]):List[Int] = {
		var IntersectionList = List[Int]()

		seq1.keys.foreach{ Transaction => if (seq2.contains(Transaction)) { IntersectionList ++= List(Transaction) } }
		return IntersectionList
	}

	def get_new_list(posList1:List[Int],posList2:List[Int]):List[Int] ={
		
		var newPosList = List[Int]()
		var poslist1 = List[Int]()
		var poslist2 = List[Int]()

		poslist1 = posList1.sorted
		poslist2 = posList2.sorted


		var min = poslist1.min
		var max = poslist2.max

		if(max < min && poslist1.length > 1 && poslist2.length > 1)
		{
			newPosList
		}

		else{
			// for each index in poslist of 2nd sequence add it to new list if its greater than the min element 
			poslist2.foreach{ position => if(position > poslist1.min) { newPosList ++= List(position) } }
			newPosList
		}
				
	}

	// A helper function to check if the 1st sequence  is a subsequence of 2nd sequence 
	def isSubSequence(Seq1:String,Seq2:String):Boolean={
		var temp:Int = 0
		for(ix <- 0 to Seq1.length-1){
			temp = Seq2.indexOf(Seq1(ix),temp)+1 				
			if(temp == 0)
				return false
		}
		return true
	}

	// Helper function to make the frequent sequence list maximal

	def get_maximal():Unit = {
		var flag:Boolean = false
		for(i <- 0 to Frequent_Sequences.length-1){
			flag = false
			
			for(j <- 0 to Frequent_Sequences.length-1;if(flag == false)){
				// Remvoe a sequence if its already a subsequenc of another frequent subsequence
				if(i!=j){
					// now if we find a sequence that is a subsequence of the 
					if(Frequent_Sequences(i).length < Frequent_Sequences(j).length){
						if(isSubSequence(Frequent_Sequences(i),Frequent_Sequences(j))){
							// printf("%s %s \n",Frequent_Sequences(i),Frequent_Sequences(j))
							flag = true
						}
					}
				}
			}

			if(flag == false){
				Maximal_Frequent_Sequences += Frequent_Sequences(i)
			}
		}
	}


	// This is the method that computes the frequent sequences 

	def Spade(minSup:Int,TidList:collection.mutable.Map[String,collection.mutable.Map[Int,List[Int]]]):collection.mutable.Map[String,collection.mutable.Map[Int,List[Int]]] ={
		
		var seqList = new ListBuffer[String]() // holds all the frequent subsequences 

		var new_TidList = collection.mutable.Map[String,collection.mutable.Map[Int,List[Int]]]()

		for((subSeq,posList)<-TidList){
				seqList += subSeq
		}


		// sort the list lexicographically
		var tempArray = new Array[String](seqList.length)
		seqList.copyToArray(tempArray)
		tempArray = tempArray.sorted

		// we will now build the new equivalence class for the next iteration 

		for(i <- 0 to tempArray.length-1){
		  for(j <- 0 to tempArray.length-1){
		  	if(shareSubseq(tempArray(i),tempArray(j))){

		  		// compute the new sequence
	  			var new_seq = tempArray(i);
				new_seq += tempArray(j)(tempArray(j).length -1 ).toString		  		

		  		// holds the intersection of the tid lists.
		  		var newTransList = List[Int]();
		  		
		  		// Obtain the intersection of the transaction id list for the sequences
	  			newTransList = Intersection_Lists(TidList(tempArray(i)),TidList(tempArray(j)))

				// This is a temporary map that holds the tuples <tid,poslist> for this sequence for each transaction id 
				var temp_transMap = collection.mutable.Map[Int,List[Int]]()  


	  			// The list now has all the transactions common to both the lists if its not empty we have to validate it
	  			if(newTransList.length > 0){

					newTransList = newTransList.sorted
							
		  			//validity check for transactions in the new tidlist, for each transaction obtain the valid poslist
		  			for(transNum <- 0 to newTransList.length-1){
		  			
		  				// This is a temporary position list that holds the new list after merging the two pos lists
		  				var temp_posList = List[Int]()	 
		  			
		  				// Merget the two poslists and we store the that in temporary posList for the new Sequence for this transaction
		  				temp_posList = get_new_list(TidList(tempArray(i))(newTransList(transNum)),TidList(tempArray(j))(newTransList(transNum)))
		  				
		  				// add to map if the length is not zero i.e. there is atleast one instance of the sequence in the transaction
		  				if(temp_posList.length > 0){
		  					temp_transMap += (transNum -> temp_posList)	
		  				}		  				
		  			}	
		  		}

		  		// Now we have the map of transactions <tid,poslist> for this new sequence so add it to the map for next transaction
		  		if(temp_transMap.size > minSup){
		  			new_TidList += (new_seq -> temp_transMap) //  add to the tid database if there is a 
		  			Frequent_Sequences += new_seq			// this is a frequent sequence 
		 			
		 			FrequentSeq_SupportCount += (new_seq -> temp_transMap.size)

		 			printf("****** Combining %s %s %s  *******\n",tempArray(i),tempArray(j),new_seq)
		 			// if(Frequent_Sequences.contains(tempArray(i)))
		 			// 	Frequent_Sequences -= tempArray(i)
		 			
		 			// if(Frequent_Sequences.contains(tempArray(j)))
		 			// 	Frequent_Sequences -= tempArray(j)
		  		}
		  	}
		  }	
		}

  		// Check if the new Equivalence class has any new members if it does go to the next iteration
/*  		if(new_TidList.size > 0){
  			Spade(minSup,new_TidList)
  		}
*/

		new_TidList
	}


	def main(args: Array[String]){

	 	printf("'%s","SPADE-An Eclat based algorithm for frequent Subsequence Mining")	
	 	printf(" Assumptions made are as follows : \n 1.%5s \n 2.%5s \n","The dataset is only of strings","The Strings are only made up of lower case alphabets")	
		println(s" Enter the path to the input file for the dataset of strings")

		var Path = readLine
		preProcess(Path)

		println(s" Enter the min support for a sub-sequence to be frequent")
		var minSup = readInt

		println(s" Enter the min Confidence for a rule to be frequent")
		var minConf = readDouble


		for((subSeq,posList)<-vertical_DB_1){
			if(posList.size < minSup){
				vertical_DB_1 -= subSeq		
			}
			else{
				Frequent_Sequences += subSeq
				FrequentSeq_SupportCount += (subSeq -> vertical_DB_1(subSeq).size)
			}
		}


		

		var temp_tid_list = Spade(minSup,vertical_DB_1)
		
		breakable
		{
			while(true)
			{
				if(temp_tid_list.size > 0)
					temp_tid_list = Spade(minSup,temp_tid_list)
				else
					break
			}
		}
			
		get_maximal()

		Frequent_Sequences = Frequent_Sequences.sorted
		printf(" The Frequent Sequence are : \n")
		Frequent_Sequences.foreach { subSequence => printf("%s %d \n",subSequence,FrequentSeq_SupportCount(subSequence))}
		printf("\n The maximal ones are : \n")
		Maximal_Frequent_Sequences.foreach { subSequence => printf("%s %d \n",subSequence,FrequentSeq_SupportCount(subSequence))}

		// closed frequent 

		// Rule Generation 
		var conf:Double = 0.0
		var Confident_Rules = collection.mutable.Map[String,collection.mutable.Map[String,String]]()
		
		Frequent_Sequences.foreach{ Freq_Sequence => 
			var Temp_RuleMap = collection.mutable.Map[String,String]()
			for(ix <- 1 to Freq_Sequence.length-1) { 
				var new_seq = Freq_Sequence.subSequence(0,Freq_Sequence.length-ix) 
				conf = FrequentSeq_SupportCount(Freq_Sequence) 
				conf /= FrequentSeq_SupportCount(new_seq.toString)  
		//		printf("\n %s %s %f \n",new_seq.toString,Freq_Sequence.subSequence(Freq_Sequence.length-ix,Freq_Sequence.length).toString,conf)
				if(conf > minConf)
					Temp_RuleMap += (new_seq.toString -> Freq_Sequence.subSequence(Freq_Sequence.length-ix,Freq_Sequence.length).toString)
			} 

			Confident_Rules += (Freq_Sequence -> Temp_RuleMap)
		}	

		printf(" \n Generating the Frequent Rules based on the given Confidence Pruning \n")
		
		// Confident_Rules.keys.foreach{ fseq => Confident_Rules(fseq).keys.foreach{ Head => printf(" %s -> %s :- %f \n",Head,Confident_Rules(fseq)(Head),(FrequentSeq_SupportCount(fseq).toFloat/FrequentSeq_SupportCount(Head))) }	}
		Confident_Rules.keys.foreach{ fseq => Confident_Rules(fseq).keys.foreach{ Head => printf(" %s -> %s \n",Head,Confident_Rules(fseq)(Head)) }	}
	}
}