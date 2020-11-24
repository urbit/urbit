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
      const { config, envelopes } = mailbox;
      const hasUnreadMention = envelopes.reduce((accum, envelope, index) => {
        if (
          'text' in envelope.letter
          && envelope.letter.text.includes(window.ship)
          && (envelopes.length - index) >= envelopes.length - (config.length - config.read)
        ) {
          return true;
        }
        return accum;
      }, false);
      if (hasUnreadMention) {
        return "mention";
      }

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


export function useGraphModule(
  graphKeys: Set<string>,
  graphs: Graphs,
  graphUnreads: Record<string, number>
): SidebarAppConfig {
  const getStatus = useCallback(
    (s: string) => {
      if((graphUnreads[s] || 0) > 0) {
        return 'unread';
      }
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
