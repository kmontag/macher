# Manages configuration for this repository.

variable "github_owner" {
  default = "kmontag"
}

variable "github_repository_name" {
  default = "macher"
}

provider "github" {
  # Owner for e.g. repository resources.
  owner = var.github_owner
}

resource "github_repository" "default" {
  name       = var.github_repository_name
  visibility = "public"

  description = "Project-aware multi-file LLM editing for Emacs, based on gptel."
  topics      = ["emacs", "gptel", "llm"]

  vulnerability_alerts = true

  # Suggest updating PR branches.
  allow_update_branch = true

  # Don't allow merge commits from PRs (they should be squashed or rebased instead).
  allow_merge_commit = false

  # Allow squash merges and use the PR body as the default commit content.
  allow_squash_merge          = true
  squash_merge_commit_title   = "PR_TITLE"
  squash_merge_commit_message = "PR_BODY"

  # Clean up branches after merge.
  delete_branch_on_merge = true

  has_downloads = true
  has_issues    = true
  has_projects  = false
  has_wiki      = false

  lifecycle {
    prevent_destroy = true
  }
}

data "github_rest_api" "rulesets" {
  endpoint = "/repos/${var.github_owner}/${github_repository.default.name}/rulesets"

  lifecycle {
    postcondition {
      condition     = self.code == 200
      error_message = "Expected status code 200, but got ${self.code}"
    }
  }
}

locals {
  # Array containing entries like:
  #
  #  {"id": 12345, "name": "some name", ...}.
  #
  rulesets = jsondecode(data.github_rest_api.rulesets.body)

  # Get the existing main ruleset ID. This will be used to import the ruleset resource.
  #
  # If the ruleset ever gets deleted for some reason, this will be `null`, and the associated import
  # block can simply be commented out temporarily.
  main_ruleset_name = "main"
  main_ruleset_id   = one([for ruleset in local.rulesets : ruleset.id if ruleset.name == local.main_ruleset_name])
}

resource "github_repository_ruleset" "main" {
  name        = local.main_ruleset_name
  repository  = github_repository.default.name
  target      = "branch"
  enforcement = "active"

  conditions {
    ref_name {
      include = ["~DEFAULT_BRANCH"]
      exclude = []
    }
  }

  rules {
    # Require bypass permission to create/delete the default branch.
    creation = true
    deletion = true

    # Don't allow merge commits.
    required_linear_history = true

    # Prevent force-pushes to the default branch.
    non_fast_forward = true

    # It would be nice to force status checks to pass before merging to main, but this complicates
    # the semantic-release commit process. There are some workarounds if needed in the future, see
    # for example https://github.com/semantic-release/git/issues/210#issuecomment-739880155.
    # required_status_checks {
    #   strict_required_status_checks_policy = true
    #   required_check {
    #     context = "validate_and_release"
    #   }
    # }
  }
  # Allow repository admins to bypass these checks.
  bypass_actors {
    actor_id    = 5 # admin
    actor_type  = "RepositoryRole"
    bypass_mode = "always"
  }
}

# Import statements allowing the entire workspace to be imported. If re-creating
# resources from scratch, some or all of these will need to be commented out.
import {
  to = github_repository.default
  id = var.github_repository_name
}

import {
  to = github_repository_ruleset.main
  id = "${github_repository.default.name}:${local.main_ruleset_id}"
}
