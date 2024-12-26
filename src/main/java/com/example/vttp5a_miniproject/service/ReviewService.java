package com.example.vttp5a_miniproject.service;

import java.io.StringReader;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.example.vttp5a_miniproject.model.Review;
import com.example.vttp5a_miniproject.repo.RedisRepo;

import jakarta.json.Json;
import jakarta.json.JsonObject;
import jakarta.json.JsonObjectBuilder;
import jakarta.json.JsonReader;

@Service
public class ReviewService {

    @Autowired
    private RedisRepo repo;

    public void addReview(String username, Integer movieId, 
                          String movieTitle, String content, Integer rating) {
        if (username == null || username.trim().isEmpty()) {
            throw new IllegalArgumentException("Username cannot be empty");
        }
        if (movieId == null) {
            throw new IllegalArgumentException("Movie ID cannot be empty");
        }
        if (content == null || content.trim().isEmpty()) {
            throw new IllegalArgumentException("Review content cannot be empty");
        }
        if (rating == null || rating < 1 || rating > 5) {
            throw new IllegalArgumentException("Rating must be between 1 and 5");
        }

        Review review = new Review(username, movieId, movieTitle, content, rating);

        JsonObjectBuilder builder = Json.createObjectBuilder()
            .add("username", review.getUsername())
            .add("movieId", review.getMovieId())
            .add("movieTitle", review.getMovieTitle())
            .add("content", review.getContent())
            .add("rating", review.getRating())
            .add("createdAt", review.getCreatedAt().toString());

        String reviewJson = builder.build().toString();
        repo.saveReview(username, movieId.toString(), reviewJson);
    }

    public Review getReview(String username, Integer movieId) {
        String reviewJson = repo.getReview(username, movieId.toString());
        if (reviewJson == null) {
            return null;
        }

        JsonReader jsonReader = Json.createReader(new StringReader(reviewJson));
        JsonObject json = jsonReader.readObject();

        Review review = new Review();
        review.setUsername(json.getString("username"));
        review.setMovieId(json.getInt("movieId"));
        review.setMovieTitle(json.getString("movieTitle"));
        review.setContent(json.getString("content"));
        review.setRating(json.getInt("rating"));
        review.setCreatedAt(LocalDateTime.parse(json.getString("createdAt")));

        return review;
    }

    public List<Review> getUserReviews(String username) {
        Map<Object, Object> reviewsMap = repo.getUserReviews(username);
        List<Review> reviews = new ArrayList<>();

        for (Object reviewJson : reviewsMap.values()) {
            JsonReader jsonReader = Json.createReader(new StringReader(reviewJson.toString()));
            JsonObject json = jsonReader.readObject();

            Review review = new Review();
            review.setUsername(json.getString("username"));
            review.setMovieId(json.getInt("movieId"));
            review.setMovieTitle(json.getString("movieTitle"));
            review.setContent(json.getString("content"));
            review.setRating(json.getInt("rating"));
            review.setCreatedAt(LocalDateTime.parse(json.getString("createdAt")));

            reviews.add(review);
        }

        return reviews;
    }

    public void deleteReview(String username, Integer movieId) {
        if (username == null || username.trim().isEmpty()) {
            throw new IllegalArgumentException("Username cannot be empty");
        }
        if (movieId == null) {
            throw new IllegalArgumentException("Movie ID cannot be empty");
        }

        repo.deleteReview(username, movieId.toString());
    }
}
