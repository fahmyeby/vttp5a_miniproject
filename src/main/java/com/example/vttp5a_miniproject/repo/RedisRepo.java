package com.example.vttp5a_miniproject.repo;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Set;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.stereotype.Repository;

@Repository
public class RedisRepo {
    @Autowired
    @Qualifier("redisTemplate")
    private RedisTemplate<String, String> template;

    // prefixes for redis keys
    private static final String user = "user:";
    private static final String watchlist = "watchlist:";
    private static final String rating = "rating:";
    private static final String review = "review:";

    // user ops

    // save user
    public void saveUser(String username, Map<String, String> userData) {
        String userKey = user + username;
        template.opsForHash().putAll(userKey, userData);
    }

    public Map<Object, Object> getUser(String username) {
        String userKey = user + username;
        return template.opsForHash().entries(userKey);
    }

    public boolean userExists(String username) {
        String userKey = user + username;
        return Boolean.TRUE.equals(template.hasKey(userKey));
    }

    public void updateUser(String username, Map<String, String> updates) {
        if (username == null || updates == null || updates.isEmpty()) {
            throw new IllegalArgumentException("Invalid update parameters");
        }

        String userKey = user + username;
        if (Boolean.FALSE.equals(template.hasKey(userKey))) {
            throw new NoSuchElementException("User not found");
        }

        template.opsForHash().putAll(userKey, updates);
    }

    public void deleteUser(String username) {
        String userKey = user + username;
        if (Boolean.FALSE.equals(template.hasKey(userKey))) {
            throw new NoSuchElementException("User not found");
        }
        template.delete(userKey);
    }

    // watchlist ops

    // save movie to watchlist
    public void saveToWatchlist(String username, String movieId, String movieData) {
        template.opsForHash().put(watchlist + username, movieId, movieData);
    }

    // remove from watchlist
    public void removeFromWatchlist(String username, String movieId) {
        Long removed = template.opsForHash().delete(watchlist + username, movieId);
        if (removed == 0) {
            throw new NoSuchElementException("Movie not found in watchlist");
        }
    }

    // get watchlist
    public Map<Object, Object> getWatchlist(String username) {
        return template.opsForHash().entries(watchlist + username);
    }

    // rating ops

    // save rating
    public void saveRating(String username, String movieId, String rating) {
        template.opsForHash().put(rating + user, movieId, rating);
    }

    // get rating
    public String getRating(String username, String movieId) {
        String getRating = (String) template.opsForHash().get(rating + username,
                movieId);
        return getRating;
    }

    // get all ratings
    public Map<Object, Object> getAllRatings(String username) {
        return template.opsForHash().entries(rating + username);
    }


    // profile update ops
    // update user profile
    public void updateUserProfile(String username, Map<String, String> updates) {
        if (username == null || updates == null || updates.isEmpty()) {
            throw new IllegalArgumentException("Invalid update parameters");
        }

        String userKey = user + username;
        if (Boolean.FALSE.equals(template.hasKey(userKey))) {
            throw new NoSuchElementException("User not found");
        }

        template.opsForHash().putAll(userKey, updates);
    }

    public void renameUser(String oldUsername, String newUsername) {
        if (userExists(newUsername)) {
            throw new IllegalStateException("New username is already taken");
        }
    
        // Retrieve user data for the old username
        Map<Object, Object> userData = getUser(oldUsername);
        if (userData == null || userData.isEmpty()) {
            throw new IllegalArgumentException("User with username '" + oldUsername + "' does not exist");
        }
    
        // Save data under the new username
        saveUser(newUsername, (Map<String, String>) (Map<?, ?>) userData);
    
        // Delete the old username's data
        template.delete(oldUsername);
    }
    

    // update username
    public void updateUsername(String oldUsername, String newUsername) {
        if (oldUsername == null || newUsername == null) {
            throw new IllegalArgumentException("Usernames cannot be null");
        }

        String oldKey = user + oldUsername;
        String newKey = user + newUsername;

        if (Boolean.FALSE.equals(template.hasKey(oldKey))) {
            throw new NoSuchElementException("User not found");
        }
        if (Boolean.TRUE.equals(template.hasKey(newKey))) {
            throw new IllegalStateException("New username already exists");
        }

        // Copy user data
        Map<Object, Object> userData = template.opsForHash().entries(oldKey);
        template.opsForHash().putAll(newKey, userData);

        // Copy watchlist
        String oldWatchlist = watchlist + oldUsername;
        String newWatchlist = watchlist + newUsername;
        Map<Object, Object> watchlistData = template.opsForHash().entries(oldWatchlist);
        if (!watchlistData.isEmpty()) {
            template.opsForHash().putAll(newWatchlist, watchlistData);
        }

        // Delete old data using pipeline for efficiency
        template.delete(oldKey);
        template.delete(oldWatchlist);
    }

    //review ops
     public void saveReview(String username, String movieId, String reviewJson) {
        String key = review + username + ":" + movieId;
        template.opsForValue().set(key, reviewJson);
    }

    public String getReview(String username, String movieId) {
        String key = review + username + ":" + movieId;
        return template.opsForValue().get(key);
    }

    public Map<Object, Object> getUserReviews(String username) {
        String pattern = review + username + ":*";
        Set<String> keys = template.keys(pattern);
        Map<Object, Object> reviews = new HashMap<>();
        if (keys != null) {
            for (String key : keys) {
                String value = template.opsForValue().get(key);
                if (value != null) {
                    reviews.put(key.split(":")[2], value);
                }
            }
        }
        return reviews;
    }

    public void deleteReview(String username, String movieId) {
        String key = review + username + ":" + movieId;
        template.delete(key);
    }

}
