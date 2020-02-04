function prune-topics
    for branch in (git branch --merged | grep topic/)
        git branch -d (echo $branch | tr -d [:space:])
    end
end
