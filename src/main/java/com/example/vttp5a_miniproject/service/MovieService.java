package com.example.vttp5a_miniproject.service;

import java.io.StringReader;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;
import com.example.vttp5a_miniproject.model.Movie;
import com.example.vttp5a_miniproject.repo.RedisRepo;

import jakarta.json.Json;
import jakarta.json.JsonArray;
import jakarta.json.JsonObject;
import jakarta.json.JsonObjectBuilder;
import jakarta.json.JsonReader;

@Service
public class MovieService {
    @Value("${api.key}")
    private String apiKey;

    @Value("${api.url}")
    private String apiUrl;

    @Autowired
    RedisRepo repo;

    private final RestTemplate restTemplate = new RestTemplate();

    public List<Movie> searchMovies(String query) {
        String encodedQuery = URLEncoder.encode(query, StandardCharsets.UTF_8);
        String url = apiUrl + "/search/movie?api_key=" + apiKey + "&query=" + encodedQuery;

        String data = restTemplate.getForObject(url, String.class);
        JsonReader jsonReader = Json.createReader(new StringReader(data));
        JsonObject jsonObject = jsonReader.readObject();
        JsonArray movies = jsonObject.getJsonArray("results");

        List<Movie> movieList = new ArrayList<>();
        for (int i = 0; i < movies.size(); i++) {
            JsonObject movieJson = movies.getJsonObject(i);
            Movie movie = new Movie();

            if (movieJson.containsKey("id")) {
                movie.setId(movieJson.getInt("id"));
            } else {
                movie.setId(0);
            }

            if (movieJson.containsKey("title")) {
                movie.setTitle(movieJson.getString("title"));
            } else {
                movie.setTitle("No Title Available");
            }

            if (movieJson.containsKey("overview")) {
                movie.setOverview(movieJson.getString("overview"));
            } else {
                movie.setOverview("No Overview Available");
            }

            if (movieJson.containsKey("release_date") && !movieJson.isNull("release_date")) {
                movie.setReleaseDate(movieJson.getString("release_date"));
            } else {
                movie.setReleaseDate("Unknown");
            }

            if (movieJson.containsKey("poster_path") && !movieJson.isNull("poster_path")) {
                movie.setPosterPath(movieJson.getString("poster_path"));
            } else {
                movie.setPosterPath("No Poster Available");
            }

            if (movieJson.containsKey("vote_average") && !movieJson.isNull("vote_average")) {
                movie.setVoteAverage(movieJson.getJsonNumber("vote_average").doubleValue());
            } else {
                movie.setVoteAverage(0.0);
            }

            movieList.add(movie);
        }
        return movieList;
    }

    public Movie getMovieById(String movieId) {
        String url = apiUrl + "/movie/" + movieId + "?api_key=" + apiKey;
        String data = restTemplate.getForObject(url, String.class);

        JsonReader jsonReader = Json.createReader(new StringReader(data));
        JsonObject movieJson = jsonReader.readObject();

        Movie movie = new Movie();

        if (movieJson.containsKey("id")) {
            movie.setId(movieJson.getInt("id"));
        } else {
            movie.setId(0);
        }

        if (movieJson.containsKey("title") && !movieJson.isNull("title")) {
            movie.setTitle(movieJson.getString("title"));
        } else {
            movie.setTitle("No Title Available");
        }

        if (movieJson.containsKey("overview") && !movieJson.isNull("overview")) {
            movie.setOverview(movieJson.getString("overview"));
        } else {
            movie.setOverview("No Overview Available");
        }

        if (movieJson.containsKey("poster_path") && !movieJson.isNull("poster_path")) {
            movie.setPosterPath(movieJson.getString("poster_path"));
        } else {
            movie.setPosterPath("");
        }

        if (movieJson.containsKey("release_date") && !movieJson.isNull("release_date")) {
            movie.setReleaseDate(movieJson.getString("release_date"));
        } else {
            movie.setReleaseDate("Unknown");
        }

        if (movieJson.containsKey("vote_average") && !movieJson.isNull("vote_average")) {
            movie.setVoteAverage(movieJson.getJsonNumber("vote_average").doubleValue());
        } else {
            movie.setVoteAverage(0.0);
        }

        return movie;
    }

    // add movie to user's watchlist - save to redis
    public void addToWatchlist(String username, String movieId) {
        // Validate inputs
        if (username == null || username.trim().isEmpty()) {
            throw new IllegalArgumentException("Username cannot be empty");
        }
        if (movieId == null || movieId.trim().isEmpty()) {
            throw new IllegalArgumentException("Movie ID cannot be empty");
        }

        try {
            Movie movie = getMovieById(movieId);
            if (movie == null) {
                throw new IllegalArgumentException("Movie not found");
            }
            JsonObjectBuilder builder = Json.createObjectBuilder();

            // Add required fields with null checks
            if (movie.getId() != null) {
                builder.add("id", movie.getId());
            } else {
                builder.add("id", "0");
            }

            if (movie.getTitle() != null) {
                builder.add("title", movie.getTitle());
            } else {
                builder.add("title", "No Title Available");
            }

            if (movie.getOverview() != null) {
                builder.add("overview", movie.getOverview());
            } else {
                builder.add("overview", "No Overview Available");
            }

            if (movie.getPosterPath() != null) {
                builder.add("poster_path", movie.getPosterPath());
            } else {
                builder.add("poster_path", "");
            }

            if (movie.getReleaseDate() != null) {
                builder.add("release_date", movie.getReleaseDate());
            } else {
                builder.add("release_date", "Unknown");
            }

            if (movie.getVoteAverage() != null) {
                builder.add("vote_average", movie.getVoteAverage());
            } else {
                builder.add("vote_average", 0.0);
            }
            String movieJson = builder.build().toString();
            repo.saveToWatchlist(username, movieId, movieJson);

        } catch (Exception e) {
            throw new RuntimeException("Failed to add movie to watchlist: " + e.getMessage());
        }
    }

    // get user's watchlist
    public List<Movie> getWatchlist(String username) {
        Map<Object, Object> watchlist = repo.getWatchlist(username);
        List<Movie> movies = new ArrayList<>();

        for (Map.Entry<Object, Object> entry : watchlist.entrySet()) {
            JsonReader jsonReader = Json.createReader(new StringReader(entry.getValue().toString()));
            JsonObject movieJson = jsonReader.readObject();

            Movie movie = new Movie();

            if (movieJson.containsKey("id") && !movieJson.isNull("id")) {
                movie.setId(movieJson.getInt("id"));
            } else {
                movie.setId(0);
            }

            if (movieJson.containsKey("title") && !movieJson.isNull("title")) {
                movie.setTitle(movieJson.getString("title"));
            } else {
                movie.setTitle("No Title Available");
            }

            if (movieJson.containsKey("overview") && !movieJson.isNull("overview")) {
                movie.setOverview(movieJson.getString("overview"));
            } else {
                movie.setOverview("No Overview Available");
            }

            if (movieJson.containsKey("poster_path") && !movieJson.isNull("poster_path")) {
                movie.setPosterPath(movieJson.getString("poster_path"));
            } else {
                movie.setPosterPath(""); // Empty string as default
            }

            if (movieJson.containsKey("release_date") && !movieJson.isNull("release_date")) {
                movie.setReleaseDate(movieJson.getString("release_date"));
            } else {
                movie.setReleaseDate("Unknown");
            }

            if (movieJson.containsKey("vote_average") && !movieJson.isNull("vote_average")) {
                movie.setVoteAverage(movieJson.getJsonNumber("vote_average").doubleValue());
            } else {
                movie.setVoteAverage(0.0); // Default to 0.0
            }

            movies.add(movie);
        }

        return movies;
    }

    // remove from watchlist - delete in redis
    public void removeFromWatchlist(String username, String movieId) {
        repo.removeFromWatchlist(username, movieId);
    }

    // get popular movies
    public List<Movie> getPopularMovies() {
        try {
            // Create URL
            String url = apiUrl + "/movie/popular";
            url += "?api_key=" + apiKey;
            url += "&language=en-US";
            url += "&page=1";
            String response = restTemplate.getForObject(url, String.class);
            if (response == null) {
                throw new RuntimeException("No response from API");
            }
            List<Movie> movieList = new ArrayList<>();
            JsonReader jsonReader = Json.createReader(new StringReader(response));
            JsonObject result = jsonReader.readObject();
            JsonArray movies = result.getJsonArray("results");
            for (int i = 0; i < movies.size(); i++) {
                try {
                    JsonObject movieJson = movies.getJsonObject(i);
                    Movie movie = new Movie();

                    movie.setId(movieJson.getInt("id", 0));
                    movie.setTitle(movieJson.getString("title", "Untitled"));
                    movie.setOverview(movieJson.getString("overview", "No overview available"));
                    movie.setPosterPath(movieJson.getString("poster_path", ""));
                    movie.setReleaseDate(movieJson.getString("release_date", ""));

                    if (movieJson.containsKey("vote_average")) {
                        movie.setVoteAverage(movieJson.getJsonNumber("vote_average").doubleValue());
                    } else {
                        movie.setVoteAverage(0.0);
                    }

                    movieList.add(movie);
                } catch (Exception e) {
                    System.err.println("Error parsing movie: " + e.getMessage());
                    continue;
                }
            }

            return movieList;

        } catch (Exception e) {
            throw new RuntimeException("Failed to fetch popular movies: " + e.getMessage());
        }
    }

    public List<Movie> getTopRatedMovies() {
        try {
            String url = apiUrl + "/movie/top_rated";
            url += "?api_key=" + apiKey;
            url += "&language=en-US";
            url += "&page=1";

            String response = restTemplate.getForObject(url, String.class);
            if (response == null) {
                throw new RuntimeException("No response from API");
            }

            List<Movie> movieList = new ArrayList<>();
            JsonReader jsonReader = Json.createReader(new StringReader(response));
            JsonObject result = jsonReader.readObject();
            JsonArray items = result.getJsonArray("results");

            for (int i = 0; i < items.size(); i++) {
                try {
                    JsonObject json = items.getJsonObject(i);
                    Movie movie = new Movie();

                    movie.setId(json.getInt("id", 0));
                    movie.setTitle(json.getString("title", "Untitled"));
                    movie.setOverview(json.getString("overview", "No overview available"));
                    movie.setPosterPath(json.getString("poster_path", ""));
                    movie.setReleaseDate(json.getString("release_date", ""));

                    if (json.containsKey("vote_average")) {
                        movie.setVoteAverage(json.getJsonNumber("vote_average").doubleValue());
                    } else {
                        movie.setVoteAverage(0.0);
                    }

                    movieList.add(movie);
                } catch (Exception e) {
                    System.err.println("Error parsing movie: " + e.getMessage());
                }
            }
            return movieList;

        } catch (Exception e) {
            throw new RuntimeException("Failed to fetch top rated movies: " + e.getMessage());
        }
    }

    public List<Movie> getPopularTVShows() {
        try {
            String url = apiUrl + "/tv/popular";
            url += "?api_key=" + apiKey;
            url += "&language=en-US";
            url += "&page=1";

            String response = restTemplate.getForObject(url, String.class);
            if (response == null) {
                throw new RuntimeException("No response from API");
            }

            List<Movie> showList = new ArrayList<>();
            JsonReader jsonReader = Json.createReader(new StringReader(response));
            JsonObject result = jsonReader.readObject();
            JsonArray items = result.getJsonArray("results");

            for (int i = 0; i < items.size(); i++) {
                try {
                    JsonObject json = items.getJsonObject(i);
                    Movie show = new Movie();

                    show.setId(json.getInt("id", 0));
                    show.setTitle(json.getString("name", "Untitled")); // TV shows use "name" instead of "title"
                    show.setOverview(json.getString("overview", "No overview available"));
                    show.setPosterPath(json.getString("poster_path", ""));
                    show.setReleaseDate(json.getString("first_air_date", "")); // TV shows use "first_air_date"

                    if (json.containsKey("vote_average")) {
                        show.setVoteAverage(json.getJsonNumber("vote_average").doubleValue());
                    } else {
                        show.setVoteAverage(0.0);
                    }

                    showList.add(show);
                } catch (Exception e) {
                    System.err.println("Error parsing TV show: " + e.getMessage());
                }
            }
            return showList;

        } catch (Exception e) {
            throw new RuntimeException("Failed to fetch popular TV shows: " + e.getMessage());
        }
    }

    public List<Movie> getTopRatedTVShows() {
        try {
            String url = apiUrl + "/tv/top_rated";
            url += "?api_key=" + apiKey;
            url += "&language=en-US";
            url += "&page=1";

            String response = restTemplate.getForObject(url, String.class);
            if (response == null) {
                throw new RuntimeException("No response from API");
            }

            List<Movie> showList = new ArrayList<>();
            JsonReader jsonReader = Json.createReader(new StringReader(response));
            JsonObject result = jsonReader.readObject();
            JsonArray items = result.getJsonArray("results");

            for (int i = 0; i < items.size(); i++) {
                try {
                    JsonObject json = items.getJsonObject(i);
                    Movie show = new Movie();

                    show.setId(json.getInt("id", 0));
                    show.setTitle(json.getString("name", "Untitled")); // TV shows use "name" instead of "title"
                    show.setOverview(json.getString("overview", "No overview available"));
                    show.setPosterPath(json.getString("poster_path", ""));
                    show.setReleaseDate(json.getString("first_air_date", "")); // TV shows use "first_air_date"

                    if (json.containsKey("vote_average")) {
                        show.setVoteAverage(json.getJsonNumber("vote_average").doubleValue());
                    } else {
                        show.setVoteAverage(0.0);
                    }

                    showList.add(show);
                } catch (Exception e) {
                    System.err.println("Error parsing TV show: " + e.getMessage());
                }
            }
            return showList;

        } catch (Exception e) {
            throw new RuntimeException("Failed to fetch top rated TV shows: " + e.getMessage());
        }
    }

    public boolean isInWatchlist(String username, String movieId) {
        try {
            Map<Object, Object> watchlist = repo.getWatchlist(username);
            return watchlist.containsKey(movieId);
        } catch (Exception e) {
            return false;
        }
    }

    

}
