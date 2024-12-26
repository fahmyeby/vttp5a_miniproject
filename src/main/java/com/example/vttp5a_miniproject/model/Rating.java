package com.example.vttp5a_miniproject.model;

import jakarta.validation.constraints.NotEmpty;
import jakarta.validation.constraints.Size;

public class Rating {
    @NotEmpty(message = "Username cannot be empty")
    private String username;

    @NotEmpty(message = "Movie ID cannot be empty")
    private String movieId;

    @NotEmpty(message = "Score input cannot be empty")
    @Size(message = "Size must be between 1 and 5", min=1, max=5)
    private Integer score;

    public Rating() {
    }

    public Rating(String username, String movieId, Integer score) {
        this.username = username;
        this.movieId = movieId;
        this.score = score;
    }

    public String getUsername() {
        return username;
    }

    public void setUsername(String username) {
        this.username = username;
    }

    public String getMovieId() {
        return movieId;
    }

    public void setMovieId(String movieId) {
        this.movieId = movieId;
    }

    public Integer getScore() {
        return score;
    }

    public void setScore(Integer score) {
        this.score = score;
    }

    @Override
    public String toString() {
        return username + ", " + movieId + ", " + score;
    }

}
