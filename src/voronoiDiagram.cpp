/*
  *  Copyright (C) 2012 Paul Murrell
*
  *  This program is free software; you can redistribute it and/or modify
*  it under the terms of the GNU General Public License as published by
*  the Free Software Foundation; either version 2 of the License, or
*  (at your option) any later version.
*
  *  This program is distributed in the hope that it will be useful,
*  but WITHOUT ANY WARRANTY; without even the implied warranty of
*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
*  GNU General Public License for more details.
*
  *  You should have received a copy of the GNU General Public License
*  along with this program; if not, a copy is available at
*  http://www.gnu.org/licenses/gpl.txt
*/

// This code is based on a CGAL example
// examples/Apollonius_graph_2/ag2_exact_traits.cpp
#include <Rcpp.h>
using namespace Rcpp;

// standard includes
#include <iostream>
#include <fstream>
#include <cassert>

// the number type
#include <CGAL/MP_Float.h>

// example that uses an exact number type

#include <CGAL/Exact_predicates_inexact_constructions_kernel.h>
#include <CGAL/Delaunay_triangulation_2.h>
#include <iterator>

typedef CGAL::Exact_predicates_inexact_constructions_kernel K;
typedef K::Point_2 Point_2;
typedef K::Iso_rectangle_2 Iso_rectangle_2;
typedef K::Segment_2 Segment_2;
typedef K::Ray_2 Ray_2;
typedef K::Line_2 Line_2;

// typedefs for the traits and the algorithm

#include <CGAL/Apollonius_graph_2.h>
#include <CGAL/Apollonius_graph_traits_2.h>

typedef CGAL::Apollonius_graph_traits_2<K>   Traits;
typedef CGAL::Apollonius_graph_2<Traits>     Apollonius_graph;


//A class to recover Voronoi diagram from stream.
struct Cropped_voronoi_from_apollonius{
  std::list<Segment_2> m_cropped_vd;
  Iso_rectangle_2 m_bbox;

  Cropped_voronoi_from_apollonius(const Iso_rectangle_2& bbox):m_bbox(bbox){}

  template <class RSL>
    void crop_and_extract_segment(const RSL& rsl){
      CGAL::Object obj = CGAL::intersection(rsl,m_bbox);
      const Segment_2* s=CGAL::object_cast<Segment_2>(&obj);
      if (s) m_cropped_vd.push_back(*s);
    }

  void operator<<(const Ray_2& ray)    { crop_and_extract_segment(ray); }
  void operator<<(const Line_2& line)  { crop_and_extract_segment(line); }
  void operator<<(const Segment_2& seg){ crop_and_extract_segment(seg); }

  void reset() {
    m_cropped_vd.erase(m_cropped_vd.begin(), m_cropped_vd.end());
  }
};
// roxygen export tag
//' @export main
// [[Rcpp::export]]
int main()
{
  std::ifstream ifs("sites.cin");
  assert( ifs );

  Apollonius_graph ag;
  Apollonius_graph::Site_2 site;

  // read the sites and insert them in the Apollonius graph
  while ( ifs >> site ) {
    ag.insert(site);
  }

  //construct a rectangle
  // This is set up to be well outside the range of the sites
  // This means that we should be able to just join up the end
  // points for any open cells, without fear of crossing the
  // area that contains the sites (EXCEPT for pretty pathological
  // cases, e.g., where there are only two sites)
  Iso_rectangle_2 bbox(-2000,-2000,2000,2000);

  Cropped_voronoi_from_apollonius vor(bbox);

  // iterate to extract Voronoi diagram edges around each vertex
  Apollonius_graph::Finite_vertices_iterator vit;
  for (vit = ag.finite_vertices_begin();
       vit != ag.finite_vertices_end();
       ++vit) {
    std::cout << "Vertex ";
    std::cout << vit->site().point();
    std::cout << "\n";
    Apollonius_graph::Edge_circulator ec = ag.incident_edges(vit), done(ec);
    if (ec != 0) {
      do {
        ag.draw_dual_edge(*ec, vor);
        // std::cout << "Edge\n";
      } while(++ec != done);
    }
    //print the cropped Voronoi diagram edges as segments
    std::copy(vor.m_cropped_vd.begin(),vor.m_cropped_vd.end(),
              std::ostream_iterator<Segment_2>(std::cout,"\n"));
    vor.reset();
  }

  //extract the entire cropped Voronoi diagram
  // ag.draw_dual(vor);

  return 0;
}
