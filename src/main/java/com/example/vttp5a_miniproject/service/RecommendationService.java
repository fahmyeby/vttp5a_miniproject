package com.example.vttp5a_miniproject.service;

import java.io.StringReader;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Set;
import java.util.stream.Collectors;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.example.vttp5a_miniproject.model.Movie;
import com.example.vttp5a_miniproject.repo.RedisRepo;

import jakarta.json.Json;
import jakarta.json.JsonObject;
import jakarta.json.JsonReader;

@Service
public class RecommendationService {

    @Autowired
    private RedisRepo repo;

    @Autowired
    private MovieService movieService;

    private static final int max = 10; // max recs
    private static final int min = 1; // min watchlist size

    public List<Movie> getRecommendations(String username) {
        validateUsername(username); // check user validity
        Map<Object, Object> watchlist = repo.getWatchlist(username); // get user watchlist
        if (watchlist == null || watchlist.size() < min) return getDefaultRecommendations(); // fallback if no watchlist

        Map<Object, Object> ratings = repo.getAllRatings(username); // get user ratings
        List<Movie> watchedMovies = new ArrayList<>(); // convert watchlist to movies
        for (Object movieData : watchlist.values()) {
            try {
                JsonReader jsonReader = Json.createReader(new StringReader(movieData.toString()));
                JsonObject json = jsonReader.readObject();
                Movie movie = new Movie(
                    json.getInt("id", 0),
                    json.getString("title", ""),
                    json.getString("overview", ""),
                    json.getString("posterPath", ""),
                    json.getString("releaseDate", ""),
                    json.containsKey("voteAverage") ? json.getJsonNumber("voteAverage").doubleValue() : 0.0
                );
                watchedMovies.add(movie);
            } catch (Exception e) {
                System.err.println("failed to parse movie data: " + e.getMessage());
            }
        }
        return generateRecommendations(watchedMovies, ratings); // generate recs
    }

    private List<Movie> getDefaultRecommendations() {
        try {
            List<Movie> popularMovies = movieService.getPopularMovies(); // get popular movies
            return popularMovies.stream().limit(max).collect(Collectors.toList()); // limit results
        } catch (Exception e) {
            throw new RuntimeException("failed to fetch default recs", e);
        }
    }

    private List<Movie> generateRecommendations(List<Movie> watchedMovies, Map<Object, Object> ratings) {
        try {
            List<Movie> allMovies = movieService.getPopularMovies(); // get all movies
            Set<Integer> watchedMovieIds = watchedMovies.stream().map(Movie::getId).collect(Collectors.toSet()); // watched ids

            List<Movie> recommendations = new ArrayList<>();
            for (Movie movie : allMovies) {
                if (!watchedMovieIds.contains(movie.getId())) {
                    Integer movieId = movie.getId();
                    if (ratings != null && ratings.containsKey(movieId)) {
                        try {
                            double rating = Double.parseDouble(ratings.get(movieId).toString());
                            if (rating >= 4.0) recommendations.add(movie); // add highly rated similar movies
                        } catch (NumberFormatException e) {
                            System.err.println("invalid rating for movie id " + movieId + ": " + e.getMessage());
                        }
                    } else recommendations.add(movie); // add if no rating data
                }
                if (recommendations.size() >= max) break; // limit recs
            }
            return recommendations;
        } catch (Exception e) {
            throw new RuntimeException("failed to generate recs", e);
        }
    }

    private void validateUsername(String username) {
        if (username == null || username.trim().isEmpty()) throw new IllegalArgumentException("username cannot be null or empty");
        if (!repo.userExists(username)) throw new NoSuchElementException("user not found: " + username);
    }
}
