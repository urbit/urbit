import { Associations, Workspace } from "~/types";

export function getTitleFromWorkspace(
  associations: Associations,
  workspace: Workspace
) {
  switch (workspace.type) {
    case "home":
      return "Home";
    case "group":
      const association = associations.contacts[workspace.group];
      return association?.metadata?.title || "";
  }
}

export function getGroupFromWorkspace(
  workspace: Workspace
): string | undefined {
  if (workspace.type === "group") {
    return workspace.group;
  }

  return undefined;
}
