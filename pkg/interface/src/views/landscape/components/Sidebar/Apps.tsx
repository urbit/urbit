import { useEffect, useCallback } from "react";
import { Inbox, ChatHookUpdate, Notebooks, Graphs } from "~/types";
import { SidebarItemStatus, SidebarAppConfig } from "./types";

export function useChat(
  inbox: Inbox,
  chatSynced: ChatHookUpdate | null
): SidebarAppConfig {
  const getStatus = useCallback(
    (s: string): SidebarItemStatus | undefined => {
      if (!(s in (chatSynced || {}))) {
        return "unsubscribed";
      }
      const mailbox = inbox?.[s];
      if (!mailbox) {
        return undefined;
      }
      const { config } = mailbox;
      if (config?.read !== config?.length) {
        return "unread";
      }
      return undefined;
    },
    [inbox, chatSynced]
  );

  const lastUpdated = useCallback(
    (s: string) => {
      const mailbox = inbox?.[s];
      if (!mailbox) {
        return 0;
      }
      return mailbox?.envelopes?.[0]?.when || 0;
    },
    [inbox]
  );

  return { lastUpdated, getStatus };
}

export function usePublish(notebooks: Notebooks): SidebarAppConfig {
  const getStatus = useCallback(
    (s: string) => {
      const [, host, name] = s.split("/");
      const notebook = notebooks?.[host]?.[name];
      if (!notebook) {
        return "unsubscribed";
      }
      if (notebook["num-unread"]) {
        return "unread";
      }
      return undefined;
    },
    [notebooks]
  );

  const lastUpdated = useCallback(
    (s: string) => {
      //  we can't get publish timestamps without loading posts
      //  so we just return the number of unreads, this ensures
      //  that unread notebooks don't get lost on the bottom
      const [, host, name] = s.split("/");
      const notebook = notebooks?.[host]?.[name];
      if(!notebook) {
        return 0;
      }
      return notebook?.["num-unread"]+1;
    },
    [notebooks]
  );

  return { getStatus, lastUpdated };
}

export function useLinks(
  graphKeys: Set<string>,
  graphs: Graphs
): SidebarAppConfig {
  const getStatus = useCallback(
    (s: string) => {
      const [, , host, name] = s.split("/");
      const graphKey = `${host.slice(1)}/${name}`;

      if (!graphKeys.has(graphKey)) {
        return "unsubscribed";
      }
      return undefined;
    },
    [graphs, graphKeys]
  );

  const lastUpdated = useCallback((s: string) => {
    // cant get link timestamps without loading posts
    const stat = getStatus(s);
    if(stat === 'unsubscribed') {
      return 0;
    }
    return 1;

  }, [getStatus]);

  return { getStatus, lastUpdated };
}
