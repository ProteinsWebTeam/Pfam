/*
  calculate minimum spanning tree for an edge list
  use an edge list, i.e., a list of pairs of numbers, tab separated, one pair per line
  vertices have to be labelled by numbers, which need not be consecutive

  Note: the edge list has to be sorted.

  usage: components [OPTIONS] edge_list

  implemenation details:
  -> parses graph twice in order to find out the number of vertices, if they 
	are not given at command line
  -> uses algorithm as given in Sedgewick, Algorithms in C, p 507 for calculating
  connected components. If two nodes joined by an edge belong to two different connected 
  components, the two components are unified and the edge is dumped. Otherwise, the edge
  is discarded.
  -> components start at 1
 */

#include <vector>
#include <map>
#include <set>

#include <iostream>
#include <iomanip>
#include <fstream>
#include <string>

#include "adda.h"

using namespace std;

extern size_t param_initial_graph_size;
extern int param_report_step;

typedef std::string Token;

#define SEPARATOR '\t'

//------------------------------------------------------------
int cadda_build_mst( const char * output_filename, 
								const char * input_filename)
{

  std::ofstream outfile( output_filename );

  std::vector< Nid > dad( param_initial_graph_size, 0 );

  std::map< Token, int > map_token2vertex;
  std::vector< Token > map_vertex2token;
  
  int iteration = 0;
  
  InStream infile(input_filename); 
  
  int noutput = 0;
  
  while (!infile.eof()) 
    {
      if (++iteration % param_report_step == 0) 
	{
	  std::cout << "## iteration=" << iteration << std::endl;
	}

      Token token1, token2;
      Score weight;
      
      infile >> token1 >> token2 >> weight;
      infile.ignore(MAX_LINE_LENGTH, '\n');
      
      // do not allow self-loops and skip header
      if (token1 == token2 ) continue;
      
      unsigned int index1, index2; 
      std::map< Token, int >::iterator it;
      
      if ( (it = map_token2vertex.find(token1)) == map_token2vertex.end()) 
	{
	  index1 = map_vertex2token.size();
	  map_token2vertex[token1] = index1;
	  map_vertex2token.push_back(token1);
	}	 	
      else 
	{
	  index1 = (*it).second;
	}
      
      if ( (it = map_token2vertex.find(token2)) == map_token2vertex.end()) 
	{
	  index2 = map_vertex2token.size();
	  map_token2vertex[token2] = index2;
	  map_vertex2token.push_back(token2);
	} 
      else 
	{
	  index2 = (*it).second;
	}
      
      if (index1 > index2) 
	{	
	  std::swap( token1, token2 );
	  std::swap( index1, index2 );
	}
      
      if (index2+1 >= dad.size() )
	dad.resize( (index2 * 2) + 1, 0);
      
      /* Sedgewick: Algo in C, p 507 */
      {
	int x,y,i,j,t;
	x = i = index1;
	y = j = index2;
	
#ifdef DEBUG
	std::cout << "before: " << token1 << "=" << index1-1 <<" " << token2 << "=" << index2-1 
		  << " " << dad[index1] << " " << dad[index2] << std::endl;
#endif 
	
	while (dad[i] > 0) i = dad[i];
	while (dad[j] > 0) j = dad[j];
	while (dad[x] > 0) 
	  {
	    t = x; x = dad[x]; dad[t] = i; 
	  }
	while (dad[y] > 0) 
	  {
	    t = y; y = dad[y]; dad[t] = j; 
	  }
	
	// nodes belong to two different components.
	if (i != j) 
	  {
	    noutput += 1;
	    outfile << token1 << SEPARATOR << token2 << SEPARATOR << weight << std::endl;
	    if (dad[j] < dad[i]) 
	      {
		dad[j] += dad[i] -1; dad[i] = j; 
	      }
	    else 
	      {
		dad[i] += dad[j] -1; dad[j] = i; 
	      }
	  }
	
#ifdef DEBUG
	std::cout << "after : " << token1 << "=" << index1-1 <<" " << token2 << "=" << index2-1 
		  << " " << dad[index1] << " " << dad[index2] << " i=" << i << " j=" << j << std::endl;
#endif 
	
      }
    }
  
  outfile << TOKEN;
  
  outfile.close();
  infile.close();
  return noutput;
}










