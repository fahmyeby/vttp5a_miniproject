package com.example.vttp5a_miniproject.controller;

import java.util.List;
import java.util.NoSuchElementException;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.servlet.mvc.support.RedirectAttributes;

import com.example.vttp5a_miniproject.model.Movie;
import com.example.vttp5a_miniproject.model.Review;
import com.example.vttp5a_miniproject.model.User;
import com.example.vttp5a_miniproject.service.MovieService;
import com.example.vttp5a_miniproject.service.RecommendationService;
import com.example.vttp5a_miniproject.service.ReviewService;
import com.example.vttp5a_miniproject.service.UserService;

import jakarta.servlet.http.HttpSession;
import jakarta.validation.Valid;

@Controller
public class WebController {
    @Autowired
    private MovieService movieService;

    @Autowired
    private UserService userService;

    @Autowired
    private RecommendationService recommendationService;

    @Autowired
    private ReviewService reviewService;

    // show home page
    @GetMapping("/")
    public String showHomepage(HttpSession session, Model model) {
        String username = (String) session.getAttribute("username");
        if (username == null) {
            return "redirect:/login";
        }

        try {
            List<Movie> popularMovies = movieService.getPopularMovies();
            List<Movie> topRatedMovies = movieService.getTopRatedMovies();
            List<Movie> popularTVShows = movieService.getPopularTVShows();
            List<Movie> topRatedTVShows = movieService.getTopRatedTVShows();
            List<Movie> recommendations = recommendationService.getRecommendations(username);

            model.addAttribute("popularMovies", popularMovies);
            model.addAttribute("topRatedMovies", topRatedMovies);
            model.addAttribute("popularTVShows", popularTVShows);
            model.addAttribute("topRatedTVShows", topRatedTVShows);
            model.addAttribute("recommendations", recommendations);
            model.addAttribute("username", username);
            return "index";
        } catch (Exception e) {
            model.addAttribute("error", "Failed to load content: " + e.getMessage());
            return "error";
        }
    }

    // auth endpoints
    @GetMapping("/login")
    public String showLoginPage(Model model) {
        model.addAttribute("user", new User());
        return "login";
    }

    @PostMapping("/login")
    public String processLogin(@Valid @ModelAttribute("user") User user, BindingResult result, HttpSession session,
            Model model) {
        if (result.hasErrors()) {
            model.addAttribute("error", "Validation error, please check input");
            return "login";
        }

        try {
            if (userService.loginUser(user.getUsername(), user.getPassword())) {
                session.setAttribute("username", user.getUsername());
                return "redirect:/";
            }
            model.addAttribute("error", "Invalid username or password");
            return "login";
        } catch (Exception e) {
            model.addAttribute("error", "Login failed: " + e.getMessage());
            return "login";
        }
    }

    @GetMapping("/register")
    public String showRegisterPage(Model model) {
        model.addAttribute("user", new User());
        return "register";
    }

    @PostMapping("/register")
    public String processRegistration(@Valid @ModelAttribute("user") User user, BindingResult result, Model model) {

        if (result.hasErrors()) {
            model.addAttribute("error", "Validation error, please check input");
            return "register";
        }
        try {
            validateRegistration(user.getUsername(), user.getPassword(), user.getEmail());
            userService.registerUser(user.getUsername(), user.getPassword(), user.getEmail());
            model.addAttribute("success", "Registration successful. Please login.");
            return "login";
        } catch (IllegalArgumentException | IllegalStateException e) {
            model.addAttribute("error", e.getMessage());
            return "register";
        }
    }

    @PostMapping("/logout")
    public String logout(HttpSession session) {
        session.invalidate();
        return "redirect:/login";
    }

    // movie related end points
    @GetMapping("/movies")
    public String showMovies(@RequestParam(required = false) String query, HttpSession session, Model model) {
        String username = (String) session.getAttribute("username");
        if (username == null) {
            return "redirect:/login";
        }
        try {
            List<Movie> recommendations = recommendationService.getRecommendations(username);
            model.addAttribute("recommendations", recommendations);
            if (query != null && !query.trim().isEmpty()) {
                List<Movie> searchResults = movieService.searchMovies(query);
                model.addAttribute("movies", searchResults);
                model.addAttribute("searchQuery", query);
            }
            model.addAttribute("username", username);
            return "movies";
        } catch (Exception e) {
            model.addAttribute("error", "Failed to fetch movies: " + e.getMessage());
            return "movies";
        }
    }

    @PostMapping("/watchlist/add")
    public String addToWatchlist(@RequestParam String movieId, HttpSession session,
            RedirectAttributes redirectAttributes) {
        String username = (String) session.getAttribute("username");
        if (username == null) {
            return "redirect:/login";
        }
        if (movieId == null || movieId.trim().isEmpty()) {
            redirectAttributes.addFlashAttribute("error", "Invalid movie ID");
            return "redirect:/movies";
        }

        try {
            movieService.addToWatchlist(username, movieId);
            redirectAttributes.addFlashAttribute("success", "Movie added to watchlist");
            return "redirect:/watchlist";
        } catch (NoSuchElementException e) {
            redirectAttributes.addFlashAttribute("error", "Movie not found");
            return "redirect:/movies";
        } catch (IllegalStateException e) {
            redirectAttributes.addFlashAttribute("error", "Movie already in watchlist");
            return "redirect:/movies";
        } catch (Exception e) {
            redirectAttributes.addFlashAttribute("error", "Failed to add movie to watchlist: " + e.getMessage());
            return "redirect:/movies";
        }
    }

    @GetMapping("/watchlist")
    public String showWatchlist(HttpSession session, Model model) {
        String username = (String) session.getAttribute("username");
        if (username == null) {
            return "redirect:/login";
        }

        try {
            List<Movie> watchlist = movieService.getWatchlist(username);
            model.addAttribute("movies", watchlist);
            return "watchlist";
        } catch (Exception e) {
            model.addAttribute("error", "Failed to load watchlist: " + e.getMessage());
            return "watchlist";
        }
    }

    @PostMapping("/watchlist/remove")
    public String removeFromWatchlist(@RequestParam String movieId, HttpSession session,
            RedirectAttributes redirectAttributes) {
        String username = (String) session.getAttribute("username");
        if (username == null) {
            return "redirect:/login";
        }

        if (movieId == null || movieId.trim().isEmpty()) {
            redirectAttributes.addFlashAttribute("error", "Invalid movie ID");
            return "redirect:/watchlist";
        }

        try {
            movieService.removeFromWatchlist(username, movieId);
            redirectAttributes.addFlashAttribute("success", "Movie removed from watchlist");
            return "redirect:/watchlist";
        } catch (NoSuchElementException e) {
            redirectAttributes.addFlashAttribute("error", "Movie not found in watchlist");
            return "redirect:/watchlist";
        } catch (Exception e) {
            redirectAttributes.addFlashAttribute("error", "Failed to remove movie: " + e.getMessage());
            return "redirect:/watchlist";
        }
    }

    // path variable used here
    @GetMapping("/movies/{id}")
    public String showMovieDetails(@PathVariable String id, HttpSession session, Model model) {
        String username = (String) session.getAttribute("username");
        if (username == null) {
            return "redirect:/login";
        }

        try {
            Movie movie = movieService.getMovieById(id);
            boolean isInWatchlist = movieService.isInWatchlist(username, id);
            Review review = reviewService.getReview(username, Integer.parseInt(id));

            model.addAttribute("movie", movie);
            model.addAttribute("inWatchlist", isInWatchlist);
            model.addAttribute("review", review);
            return "movie-details";
        } catch (Exception e) {
            model.addAttribute("error", "Failed to load movie details: " + e.getMessage());
            return "redirect:/movies";
        }
    }

    @GetMapping("/profile")
    public String showUserProfile(HttpSession session, Model model) {
        String username = (String) session.getAttribute("username");
        if (username == null) {
            return "redirect:/login";
        }

        User user = userService.getUserProfile(username);
        model.addAttribute("user", user);
        return "profile";
    }

    @GetMapping("/profile/edit")
    public String showEditProfileForm(HttpSession session, Model model) {
        String username = (String) session.getAttribute("username");
        if (username == null) {
            return "redirect:/login";
        }

        User user = userService.getUserProfile(username);
        model.addAttribute("user", user);
        return "profile-edit";
    }

    @PostMapping("/profile/edit")
    public String updateUserProfile(
            @RequestParam String username,
            @RequestParam(required = false) String newUsername,
            @RequestParam(required = false) String newEmail,
            @RequestParam(required = false) String newPassword,
            HttpSession session,
            RedirectAttributes redirectAttributes) {
        try {
            String sessionUsername = (String) session.getAttribute("username");

            // prevent mismatch btw session and username
            if (!username.equals(sessionUsername)) {
                session.invalidate();
                return "redirect:/login";
            }
            userService.updateUserProfile(username, newUsername, newEmail, newPassword);

            // update session attribute
            if (newUsername != null && !newUsername.trim().isEmpty() && !newUsername.equals(username)) {
                session.setAttribute("username", newUsername);
            }

            redirectAttributes.addFlashAttribute("success", "Profile updated successfully.");
            return "redirect:/profile";

        } catch (Exception e) {
            redirectAttributes.addFlashAttribute("error", "Failed to update profile: " + e.getMessage());
            return "redirect:/profile/edit";
        }
    }

    @PostMapping("/profile/delete")
    public String deleteUserProfile(
            @RequestParam String username,
            @RequestParam String password,
            HttpSession session,
            RedirectAttributes redirectAttributes) {
        try {
            String sessionUsername = (String) session.getAttribute("username");

            // check if session match username deleted
            if (!username.equals(sessionUsername)) {
                session.invalidate();
                return "redirect:/login";
            }

            // delete user
            userService.deleteUser(username, password);
            session.invalidate();
            redirectAttributes.addFlashAttribute("success", "Account deleted successfully.");
            return "redirect:/login";

        } catch (Exception e) {
            redirectAttributes.addFlashAttribute("error", "Failed to delete account: " + e.getMessage());
            return "redirect:/profile/edit";
        }
    }

    @GetMapping("/movies/{movieId}/review")
    public String showReviewForm(@PathVariable Integer movieId, HttpSession session, Model model) {
        String username = (String) session.getAttribute("username");
        if (username == null) {
            return "redirect:/login";
        }

        Movie movie = movieService.getMovieById(movieId.toString());
        Review existingReview = reviewService.getReview(username, movieId);

        model.addAttribute("movie", movie);
        model.addAttribute("review", existingReview);
        return "review-form";
    }

    // path variable used here
    @PostMapping("/movies/{movieId}/review")
    public String submitReview(@PathVariable Integer movieId, @RequestParam String content,
            @RequestParam Integer rating, HttpSession session, RedirectAttributes redirectAttributes) {
        String username = (String) session.getAttribute("username");
        if (username == null) {
            return "redirect:/login";
        }

        try {
            Movie movie = movieService.getMovieById(movieId.toString());
            reviewService.addReview(username, movieId, movie.getTitle(),
                    content, rating);
            redirectAttributes.addFlashAttribute("success", "Review submitted successfully");
            return "redirect:/movies/" + movieId;
        } catch (Exception e) {
            redirectAttributes.addFlashAttribute("error", "Failed to submit review: " + e.getMessage());
            return "redirect:/movies/" + movieId + "/review";
        }
    }

    @GetMapping("/reviews")
    public String showUserReviews(HttpSession session, Model model) {
        String username = (String) session.getAttribute("username");
        if (username == null) {
            return "redirect:/login";
        }

        List<Review> reviews = reviewService.getUserReviews(username);
        model.addAttribute("reviews", reviews);
        return "reviews";
    }

    // path variable used here
    @PostMapping("/movies/{movieId}/review/delete")
    public String deleteReview(@PathVariable Integer movieId, HttpSession session,
            RedirectAttributes redirectAttributes) {
        String username = (String) session.getAttribute("username");
        if (username == null) {
            return "redirect:/login";
        }

        try {
            reviewService.deleteReview(username, movieId);
            redirectAttributes.addFlashAttribute("success", "Review deleted successfully");
            return "redirect:/reviews";
        } catch (Exception e) {
            redirectAttributes.addFlashAttribute("error", "Failed to delete review: " + e.getMessage());
            return "redirect:/movies/" + movieId;
        }
    }

    private void validateRegistration(String username, String password, String email) {
        if (username == null || username.trim().isEmpty()) {
            throw new IllegalArgumentException("Username cannot be empty");
        }
        if (password == null || password.trim().isEmpty()) {
            throw new IllegalArgumentException("Password cannot be empty");
        }
        if (email == null || !email.contains("@")) {
            throw new IllegalArgumentException("Invalid email format");
        }
    }
}
