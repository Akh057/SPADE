import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object preProcessor
{
	var DB = ArrayBuffer[String]();

	def extract():Unit=
	{
		for(line<-Source.fromFile("Dataset.txt").getLines()){
			DB += line;
		}		
	}

	var vertical_DB_1 = collection.mutable.Map[String, collection.mutable.Map[Int,List[Int]]]();
	var index = 0;

	def build():Unit=
	{
		for(i<-0 to DB.length-1)
		{
			for(j<-0 to DB(i).length - 1)
			{
				if(vertical_DB_1.contains(DB(i)(j).toString))
				{
					if(vertical_DB_1(DB(i)(j).toString).contains(i))
					{
						(vertical_DB_1(DB(i)(j).toString))(i) ++= List(j);
						// printf("0 Debug: %d %d\n",i,j);

					}
					else
					{
						var temp_1 = List[Int]();
						temp_1 ++= List(j);
						vertical_DB_1(DB(i)(j).toString)(i) = temp_1;

					}
				}
				else
				{
					var temp = collection.mutable.Map[Int,List[Int]]();
					var temp_1 = List[Int]();
					temp_1 ++= List(j);
					temp(i) = temp_1;
					vertical_DB_1(DB(i)(j).toString) = temp;
				}
			}
		}
	}


	def main(args: Array[String]){
		this.extract();
		this.build();
		for((k_1,v_1)<-vertical_DB_1){
			printf("Char: %s :\n",k_1);
			for((k_2,v_2)<-v_1){
				printf("List %d : ",k_2);
				for(v_3<-v_2){
					printf("%d ",v_3);
				}
				printf("\n");
			}
			printf("\n\n");
		}
	}
	//for((k,v)<-vertical_DB_1)

	// printf("Reached\n");

}