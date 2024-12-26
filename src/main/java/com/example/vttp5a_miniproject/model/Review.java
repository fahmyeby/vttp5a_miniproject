package com.example.vttp5a_miniproject.model;

import java.time.LocalDateTime;

public class Review {
    private Integer id;
    private String username;
    private Integer movieId;
    private String movieTitle;
    private String content;
    private Integer rating;
    private LocalDateTime createdAt;
    public Review() {}

    public Review(String username, Integer movieId, String movieTitle, 
                 String content, Integer rating) {
        this.username = username;
        this.movieId = movieId;
        this.movieTitle = movieTitle;
        this.content = content;
        this.rating = rating;
        this.createdAt = LocalDateTime.now();
    }

    public Integer getId() {
        return id;
    }

    public void setId(Integer id) {
        this.id = id;
    }

    public String getUsername() {
        return username;
    }

    public void setUsername(String username) {
        this.username = username;
    }

    public Integer getMovieId() {
        return movieId;
    }

    public void setMovieId(Integer movieId) {
        this.movieId = movieId;
    }

    public String getMovieTitle() {
        return movieTitle;
    }

    public void setMovieTitle(String movieTitle) {
        this.movieTitle = movieTitle;
    }

    public String getContent() {
        return content;
    }

    public void setContent(String content) {
        this.content = content;
    }

    public Integer getRating() {
        return rating;
    }

    public void setRating(Integer rating) {
        this.rating = rating;
    }

    public LocalDateTime getCreatedAt() {
        return createdAt;
    }

    public void setCreatedAt(LocalDateTime createdAt) {
        this.createdAt = createdAt;
    }

    @Override
    public String toString() {
        return id + ", " + username + ", " + movieId + ", " + movieTitle
                + ", " + content + ", " + rating + ", " + createdAt;
    }

    
}
