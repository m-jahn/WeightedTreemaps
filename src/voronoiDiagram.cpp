// This code is based on a CGAL example
// examples/Apollonius_graph_2/ag2_exact_traits.cpp
#include <Rcpp.h>
using namespace Rcpp;

// standard includes
#include <iostream>
#include <fstream>
#include <cassert>
#include <csignal>
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

typedef CGAL::Apollonius_graph_traits_2<K> Traits;
typedef CGAL::Apollonius_graph_2<Traits> Apollonius_graph;

//A class to recover Voronoi diagram from stream.
struct Cropped_voronoi_from_apollonius
{
  std::list<Segment_2> m_cropped_vd;
  Iso_rectangle_2 m_bbox;

  Cropped_voronoi_from_apollonius(const Iso_rectangle_2 &bbox) : m_bbox(bbox) {}

  template <class RSL>
  void crop_and_extract_segment(const RSL &rsl)
  {
    CGAL::Object obj = CGAL::intersection(rsl, m_bbox);
    const Segment_2 *s = CGAL::object_cast<Segment_2>(&obj);
    if (s)
      m_cropped_vd.push_back(*s);
  }

  void operator<<(const Ray_2 &ray) { crop_and_extract_segment(ray); }
  void operator<<(const Line_2 &line) { crop_and_extract_segment(line); }
  void operator<<(const Segment_2 &seg) { crop_and_extract_segment(seg); }

  void reset()
  {
    m_cropped_vd.erase(m_cropped_vd.begin(), m_cropped_vd.end());
  }
};

// custom ordering function
IntegerVector order(NumericVector x) {
  NumericVector sorted = clone(x).sort();
  return match(sorted, x)-1;
}

// custom function to find boundary position
IntegerVector get_boundary_pos(NumericVector x, NumericVector y){
  IntegerVector ind = seq(0, x.size()-1);
  IntegerVector hit = ind[abs(x) == 4000 | abs(y) == 4000];
  return hit;
}

// roxygen tags
//' cropped_voronoi
//' 
//' Tesselates a plane using a set of XY coordinates
//' 
//' @param sites (numeric matrix) The only input parameter for the function.
//'   A matrix with 3 columns: X and Y coordinates, as well as weights 
//'   that are used for tesselation.
//'   
//' @return A list of cell coordinates; one cell for each
//'   set of input coordinates.
//'   
//' @details The function is only intended for
//'   internal use. However, one can also use it directly for 
//'   test purposes.
//'   
//' @export cropped_voronoi
// [[Rcpp::export]]
SEXP cropped_voronoi(NumericMatrix sites)
{

  Apollonius_graph ag;

  // read the sites and insert them in the Apollonius graph
  std::vector<Apollonius_graph::Vertex_handle> handles;
  for (int i = 0; i != sites.nrow(); i++)
  {
    auto site = Apollonius_graph::Site_2(Point_2(sites(i, 0), sites(i, 1)), sites(i, 2));
    auto handle = ag.insert(site);
    handles.push_back(handle);
  }
  if (!ag.is_valid()) {
      ag.is_valid(true); // verbose
      Rcpp::Rcout << "Invalid apollonius graph found" << std::endl;
      return R_NilValue;
  }

  //construct a rectangle
  // This is set up to be well outside the range of the sites
  // This means that we should be able to just join up the end
  // points for any open cells, without fear of crossing the
  // area that contains the sites (EXCEPT for pretty pathological
  // cases, e.g., where there are only two sites)
  Iso_rectangle_2 bbox(-4000, -4000, 4000, 4000);

  Cropped_voronoi_from_apollonius vor(bbox);

  List results;
  // iterate to extract Voronoi diagram edges around each vertex
  for (Apollonius_graph::Vertex_handle h : handles)
  {
    if (h == NULL || !h->is_valid()) {
      Rcpp::Rcout << "Invalid apollonius graph found" << std::endl;
      return R_NilValue;
    }
    Apollonius_graph::Edge_circulator ec = ag.incident_edges(h), done(ec);
    if (ec != 0)
    {
      do
      {
        ag.draw_dual_edge(*ec, vor);
      } while (++ec != done);
    }
    // collect the cropped Voronoi diagram edges as single points of polygon
    NumericVector xs;
    NumericVector ys;
    for (auto &s : vor.m_cropped_vd)
    { 
      xs.push_back(s.source().x());
      ys.push_back(s.source().y());
      xs.push_back(s.target().x());
      ys.push_back(s.target().y());
    }
    
    // stop and return NULL if polygon is empty
    if (xs.size() == 0) {
      Rcpp::Rcout << "Invalid apollonius graph found" << std::endl;
      return R_NilValue;
    }
    
    // remove duplicate polygon points or NAs
    LogicalVector dupl = duplicated(round(xs, 2)) & duplicated(round(ys, 2));
    LogicalVector ends = floor(xs) == 3999 | floor(ys) == 3999;
    LogicalVector NAs = is_na(xs) | is_na(ys);
    xs = xs[!(dupl | ends | NAs)];
    ys = ys[!(dupl | ends | NAs)];
    
    // for sorting polygon boundary points, determine angle from center
    // first calculate deltas
    auto &vertex = h->site().point();
    NumericVector x_delta = xs - vertex.x();
    NumericVector y_delta = ys - vertex.y();
    // resolve angle, in radians
    NumericVector angle;
    for (int i = 0; i < x_delta.size(); i++)
    {
      angle.push_back(atan2(y_delta[i], x_delta[i]));
    }
    
    // check for straight lines as polygon border. These can not be ordered from
    // center and have random boundary positions; simply order by y value.
    NumericVector which_unique = unique(round(angle, 2));
    if (which_unique.size() <= 2) {
      IntegerVector ord = order(ys);
      xs = xs[ord]; ys = ys[ord];
    }
    else {
      IntegerVector ord = order(angle);
      xs = xs[ord]; ys = ys[ord];
    }
    
    // If this is a polygon touching the boundaries, rearrange points
    // starting with the first point on boundary, but without re-sorting df.
    // First find positions of boundary points
    IntegerVector boundary_pos = get_boundary_pos(xs, ys);
    // then reorder only if boundary positions are not at the ends of the vector.
    if (boundary_pos.length() == 2) {
      if (boundary_pos[0] != 0 | boundary_pos[1] != xs.length()-1) {
        IntegerVector seq1, seq2;
        seq1 = seq(boundary_pos[1], xs.length()-1);
        seq2 = seq(0, boundary_pos[1]-1);
        for (int i = 0; i < seq2.size(); i++)
        {
          seq1.push_back(seq2[i]);
        }
        xs = xs[seq1]; ys = ys[seq1];
      }
    }
    
    // collect in data frame
    DataFrame border = DataFrame::create(
      Named("x") = xs,
      Named("y") = ys
    );
    
    List res = List::create(
      Named("vertex") = NumericVector::create(vertex.x(), vertex.y()),
      Named("border") = border);
    results.push_back(res);
    
    vor.reset();
  }

  //extract the entire cropped Voronoi diagram
  return results;
}
