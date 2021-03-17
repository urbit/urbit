import { Association, resourceFromPath } from "@urbit/api";
import { useGroupForAssoc } from "../state/group";

export function usePermalinkForGraph(assoc: Association, index = "") {
  const group = usePermalinkForAssociatedGroup(assoc);
  const { ship, name } = resourceFromPath(assoc.resource);
  return `${group}/graph/${ship}/${name}${index}`;
}

function usePermalinkForAssociatedGroup(assoc: Association) {
  const group = useGroupForAssoc(assoc);
  const mod = assoc.metadata.module;
  const { ship, name } = resourceFromPath(assoc.group);
  if (!group?.hidden) {
    return `web+urbit://group/${ship}/${name}`;
  }
  if (mod === "chat") {
    return `web+urbit://messages`;
  }
  return `web+urbit://mychannel`;
}
