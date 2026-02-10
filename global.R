library(shiny)
library(visNetwork)
library(igraph)
library(dplyr)
library(tibble)

# File for persistence
DATA_FILE <- "network_data.rds"

# --- Mock Data Generation ---
generate_mock_data <- function() {
    # 1. Define People
    people_names <- c(
        "Alice", "Bob", "Charlie", "David", "Eve", "Frank", "Grace", "Heidi",
        "Ivan", "Judy", "Mallory", "Niaj", "Olivia", "Peggy", "Rupert", "Sybil",
        "Trent", "Victor", "Walter", "Zoe"
    )

    nodes_people <- tibble(
        id = people_names,
        label = people_names,
        group = "Person",
        title = paste("Person:", people_names),
        value = 1 # Initial size
    )

    # 2. Define Expertise
    expertise_areas <- c(
        "R", "Python", "Shiny", "Machine Learning", "Data Viz",
        "Project Mgmt", "Communication", "SQL", "Cloud Computing", "Statistics"
    )

    nodes_expertise <- tibble(
        id = expertise_areas,
        label = expertise_areas,
        group = "Expertise",
        title = paste("Skill:", expertise_areas),
        value = 3 # Make expertise nodes slightly larger by default
    )

    nodes <- bind_rows(nodes_people, nodes_expertise)

    # 3. Create Edges

    # a) Person -> Expertise (has_skill)
    # Each person has 1-4 random skills
    edges_skills <- list()
    for (person in people_names) {
        num_skills <- sample(1:4, 1)
        skills <- sample(expertise_areas, num_skills)
        for (skill in skills) {
            edges_skills[[length(edges_skills) + 1]] <- tibble(
                from = person,
                to = skill,
                type = "has_skill",
                color = "gray",
                weight = runif(1, 0.1, 1.0) # Random weight 0.1 to 1.0
            )
        }
    }
    edges_skills_df <- bind_rows(edges_skills)

    # b) Person -> Person (knows/colleague)
    # Random connections between people
    edges_people <- list()
    for (person in people_names) {
        # Connect to 0-3 other people
        num_contacts <- sample(0:3, 1)
        contacts <- sample(setdiff(people_names, person), num_contacts)
        for (contact in contacts) {
            # Undirected mostly, but let's store as directed for visNetwork
            edges_people[[length(edges_people) + 1]] <- tibble(
                from = person,
                to = contact,
                type = "knows",
                color = "lightblue",
                weight = runif(1, 0.1, 1.0) # Random weight
            )
        }
    }
    edges_people_df <- bind_rows(edges_people)

    edges <- bind_rows(edges_skills_df, edges_people_df)

    return(list(nodes = nodes, edges = edges))
}

# --- Persistence Functions ---
save_network_data <- function(nodes, edges) {
    saveRDS(list(nodes = nodes, edges = edges), file = DATA_FILE)
}

load_network_data <- function() {
    if (file.exists(DATA_FILE)) {
        return(readRDS(DATA_FILE))
    } else {
        data <- generate_mock_data()
        save_network_data(data$nodes, data$edges)
        return(data)
    }
}

# --- Analysis Helper Functions ---

# Calculate metrics (Degree, Betweenness) and add to nodes
calculate_metrics <- function(nodes, edges) {
    # Create igraph object
    g <- graph_from_data_frame(d = edges, vertices = nodes, directed = FALSE)

    # Degree centrality (number of connections)
    deg <- degree(g)

    # Betweenness centrality (bridgeiness)
    bet <- betweenness(g, normalized = TRUE)

    # Update nodes with metrics
    nodes_with_metrics <- nodes %>%
        mutate(
            degree = deg[id],
            betweenness = round(bet[id], 4),
            # Scale value (size) based on degree for visualization
            value = degree * 2 + 5
        )

    return(nodes_with_metrics)
}
