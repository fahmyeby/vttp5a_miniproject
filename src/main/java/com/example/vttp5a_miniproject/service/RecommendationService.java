package com.example.vttp5a_miniproject.service;

import java.io.StringReader;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
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

    private static final int max = 10; // max recommendations
    private static final int min = 1; // min watchlist size

    // get recommendations based on title similarity
    public List<Movie> getRecommendations(String username) {
        validateUsername(username); // check user validity
        Map<Object, Object> watchlist = repo.getWatchlist(username); // get watchlist
        if (watchlist == null || watchlist.size() < min) return getDefaultRecommendations(); // fallback if no watchlist

        // extract keywords from all watchlist movies
        List<String> keywords = extractKeywords(watchlist);

        // fetch recommendations based on all keywords
        return generateRecommendationsByTitle(keywords, watchlist);
    }

    // get default recommendations (popular movies)
    private List<Movie> getDefaultRecommendations() {
        try {
            List<Movie> popularMovies = movieService.getPopularMovies(); // fetch popular movies
            return popularMovies.stream().limit(max).collect(Collectors.toList()); // limit to max
        } catch (Exception e) {
            throw new RuntimeException("failed to fetch default recs", e);
        }
    }

    // generate recommendations based on title similarity
    private List<Movie> generateRecommendationsByTitle(List<String> keywords, Map<Object, Object> watchlist) {
        List<Movie> recommendations = new ArrayList<>();
        Set<Integer> watchedMovieIds = watchlist.keySet().stream()
                .map(id -> Integer.parseInt(id.toString()))
                .collect(Collectors.toSet()); // get watched ids

        int moviesPerKeyword = max / keywords.size(); // distribute recommendations across keywords
        for (String keyword : keywords) {
            try {
                List<Movie> searchResults = movieService.searchMovies(keyword); // search by keyword
                for (Movie movie : searchResults) {
                    if (!watchedMovieIds.contains(movie.getId()) && !recommendations.contains(movie)) {
                        recommendations.add(movie); // add if not watched and not already in list
                        if (recommendations.size() >= max) break; // limit total recommendations
                    }
                }
            } catch (Exception e) {
                System.err.println("error fetching recommendations for keyword " + keyword + ": " + e.getMessage());
            }
        }
        return recommendations.stream().distinct().limit(max).collect(Collectors.toList()); // ensure unique movies
    }

    // extract keywords from watchlist titles
    private List<String> extractKeywords(Map<Object, Object> watchlist) {
        Set<String> keywords = new HashSet<>();
        for (Object movieData : watchlist.values()) {
            try {
                JsonReader jsonReader = Json.createReader(new StringReader(movieData.toString()));
                JsonObject json = jsonReader.readObject();
                String title = json.getString("title");

                String[] tokens = title.split("\\s+"); // split title into words
                for (String token : tokens) {
                    if (!isStopWord(token)) keywords.add(token.toLowerCase()); // add if not stop word
                }
            } catch (Exception e) {
                System.err.println("error extracting keywords: " + e.getMessage());
            }
        }
        return new ArrayList<>(keywords);
    }

    // check if a word is a stop word
    private boolean isStopWord(String word) {
        List<String> stopWords = Arrays.asList(
            "a", "an", "the", "and", "or", "but", "nor", "so", "yet", "for",
            "in", "on", "at", "by", "with", "about", "against", "between", "into", 
            "through", "during", "before", "after", "above", "below", "to", "from", 
            "up", "down", "over", "under", "again", "further", "is", "are", "was", 
            "were", "be", "been", "being", "have", "has", "had", "do", "does", 
            "did", "i", "me", "my", "myself", "we", "our", "ours", "ourselves", 
            "you", "your", "yours", "yourself", "yourselves", "he", "him", "his", 
            "himself", "she", "her", "hers", "herself", "it", "its", "itself", 
            "they", "them", "their", "theirs", "themselves", "this", "that", 
            "these", "those", "very", "too", "quite", "just", "not", "all", "any", 
            "both", "each", "few", "more", "most", "some", "such", "no", "none", 
            "only", "as", "if", "then", "else", "when", "where", "why", "how", 
            "ii", "iii", "part", "episode", "chapter", "volume", "series", 
            "installment", "movie", "film", "story", "saga", "tale", "adventure", 
            "quest", "new", "old", "next", "last", "final", "beginning", "end", 
            "of", "for", "to", "with", "in"
        );
        return stopWords.contains(word.toLowerCase());
    }

    // validate user
    private void validateUsername(String username) {
        if (username == null || username.trim().isEmpty()) throw new IllegalArgumentException("username cannot be null or empty");
        if (!repo.userExists(username)) throw new NoSuchElementException("user not found: " + username);
    }
}


