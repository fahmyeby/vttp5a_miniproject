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

    private static final int max = 10;
    private static final int min = 1;

    public List<Movie> getRecommendations(String username) {
        validateUsername(username);
        Map<Object, Object> watchlist = repo.getWatchlist(username);
        if (watchlist == null || watchlist.size() < min) {
            return getDefaultRecommendations();
        }
        Map<Object, Object> ratings = repo.getAllRatings(username);
        List<Movie> watchedMovies = convertWatchlistToMovies(watchlist);
        return generateRecommendations(watchedMovies, ratings);
    }

    private List<Movie> getDefaultRecommendations() {
        try {
            List<Movie> popularMovies = movieService.getPopularMovies();
            return popularMovies.stream()
                    .limit(max)
                    .collect(Collectors.toList());
        } catch (Exception e) {
            throw new RuntimeException("Failed to fetch default recommendations", e);
        }
    }

    private List<Movie> generateRecommendations(List<Movie> watchedMovies, Map<Object, Object> ratings) {
        try {
            List<Movie> allMovies = movieService.getPopularMovies();
            Set<Integer> watchedMovieIds = watchedMovies.stream()
                    .map(Movie::getId)
                    .collect(Collectors.toSet());

            List<Movie> recommendations = new ArrayList<>();

            for (Movie movie : allMovies) {
                if (!watchedMovieIds.contains(movie.getId()) && isSimilarToWatchedMovies(movie, ratings)) {
                    recommendations.add(movie);
                }
                if (recommendations.size() >= max) {
                    break;
                }
            }

            return recommendations;
        } catch (Exception e) {
            throw new RuntimeException("Failed to generate recommendations", e);
        }
    }

    private boolean isSimilarToWatchedMovies(Movie movie, Map<Object, Object> ratings) {
        Integer movieId = movie.getId();
        if (ratings != null && ratings.containsKey(movieId)) {
            try {
                double rating = Double.parseDouble(ratings.get(movieId).toString());
                return rating >= 4.0;
            } catch (NumberFormatException e) {
                System.err.println("Invalid rating for movie ID " + movieId + ": " + e.getMessage());
            }
        }
        return true;
    }

    private List<Movie> convertWatchlistToMovies(Map<Object, Object> watchlist) {
        List<Movie> movies = new ArrayList<>();
        for (Object movieData : watchlist.values()) {
            try {
                Movie movie = convertJsonToMovie(movieData.toString());
                movies.add(movie);
            } catch (Exception e) {
                System.err.println("Failed to parse movie data: " + e.getMessage());
            }
        }
        return movies;
    }

    private void validateUsername(String username) {
        if (username == null || username.trim().isEmpty()) {
            throw new IllegalArgumentException("Username cannot be null or empty");
        }
        if (!repo.userExists(username)) {
            throw new NoSuchElementException("User not found: " + username);
        }
    }

    private Movie convertJsonToMovie(String jsonStr) {
        try {
            JsonReader jsonReader = Json.createReader(new StringReader(jsonStr));
            JsonObject json = jsonReader.readObject();

            return new Movie(
                json.getInt("id", 0),
                json.getString("title", ""),
                json.getString("overview", ""),
                json.getString("posterPath", ""),
                json.getString("releaseDate", ""),
                json.containsKey("voteAverage") ? json.getJsonNumber("voteAverage").doubleValue() : 0.0
            );
        } catch (Exception e) {
            throw new RuntimeException("Failed to parse movie JSON", e);
        }
    }
}
