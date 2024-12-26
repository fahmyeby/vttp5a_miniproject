package com.example.vttp5a_miniproject.restcontroller;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.example.vttp5a_miniproject.model.Movie;
import com.example.vttp5a_miniproject.service.MovieService;

@RestController
@RequestMapping("/api/movies")
public class MovieRestController {
    @Autowired MovieService movieService;

    // search for movies
    // GET /api/movies/search?query=movieTitle
    @GetMapping("/search")
    public ResponseEntity<List<Movie>> searchMovies (@RequestParam String query){
        List<Movie> movies = movieService.searchMovies(query);
        if (movies.isEmpty()){
            return ResponseEntity.noContent().build();
        } else {
            return ResponseEntity.ok().body(movies);
        }
    }

    // get movies by id
    // GET /api/movies/{id}
    @GetMapping("/{movieId}")
    public ResponseEntity<Movie> getMovieById(@PathVariable String movieId){
        Movie movie = movieService.getMovieById(movieId);
        if(movie==null){
            return ResponseEntity.notFound().build();
        } else {
            return ResponseEntity.ok().body(movie);
        }
    }

    
}
