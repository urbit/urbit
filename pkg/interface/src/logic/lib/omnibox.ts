import { Associations, isChannelAdmin, cite, Rolodex } from '@urbit/api';

interface OmniboxResult {
  title: string;
  link: string;
  app: string;
  host: string | null;
}

const result = (title, link, app, host): OmniboxResult => ({
  'title': title,
  'link': link,
  'app': app,
  'host': host
});

const shipIndex = (contacts): OmniboxResult[] => {
  const ships: OmniboxResult[] = [];
  Object.keys(contacts).map((e) => {
    return ships.push(result(e, `/~profile/${e}`, 'profile', contacts[e]?.status || ""));
  });
  return ships;
};

const commandIndex = (currentGroup, groups, associations): OmniboxResult[] => {
  // commands are special cased for default suite
  const commands: OmniboxResult[] = [];
  const group = currentGroup ? groups[currentGroup] : null;
  const association = currentGroup ? associations?.groups?.[currentGroup] : null;
  const canAdd =
    (group && association)
    ? (association.metadata.vip === 'member-metadata' || isChannelAdmin(group, currentGroup))
    : !currentGroup; // home workspace or hasn't loaded
  const workspace = currentGroup || '/home';
  commands.push(result(`Groups: Create`, `/~landscape/new`, 'Groups', null));
  if (canAdd) {
    commands.push(result(`Channel: Create`, `/~landscape${workspace}/new`, 'Groups', null));
  }
  commands.push(result(`Groups: Join`, `/~landscape/join`, 'Groups', null));

  return commands;
};

const appIndex = (apps): OmniboxResult[] => {
  // all apps are indexed from launch data
  // indexed into 'apps'
  const applications: OmniboxResult[] = [];
  Object.keys(apps)
    .filter((e) => {
      return apps[e]?.type?.basic;
    })
    .sort((a, b) => {
      return a.localeCompare(b);
    })
    .map((e) => {
      const obj = result(
        apps[e].type.basic.title,
        apps[e].type.basic.linkedUrl,
        apps[e].type.basic.title,
        null
      );
      applications.push(obj);
    });
  return applications;
};

const otherIndex = (): OmniboxResult[] => {
  const other: OmniboxResult[] = [];
  other.push(result('My Channels', '/~landscape/home', 'home', null));
  other.push(result('Notifications', '/~notifications', 'inbox', null));
  other.push(result('Profile and Settings', `/~profile/~${window.ship}`, 'profile', null));
  other.push(result('Messages', '/~landscape/messages', 'messages', null));
  other.push(result('Log Out', '/~/logout', 'logout', null));
  return other;
};

export default function index(contacts: Rolodex, associations: Associations, apps, currentGroup, groups): Map<string, OmniboxResult[]> {
  const indexes: Map<string, OmniboxResult[]> = new Map([
    ['ships', []],
    ['commands', []],
    ['subscriptions', []],
    ['groups', []],
    ['apps', []],
    ['other', []]
  ]);
  
  indexes.set('ships', shipIndex(contacts));
  // all metadata from all apps is indexed
  // into subscriptions and landscape
  const subscriptions: OmniboxResult[] = [];
  const landscape: OmniboxResult[] = [];
  Object.keys(associations).filter((e) => {
    // skip apps with no metadata
    return Object.keys(associations[e]).length > 0;
    }).map((e) => {
      // iterate through each app's metadata object
      Object.keys(associations[e]).map((association) => {
        const each = associations[e][association];
        let title = each.resource;
        if (each.metadata.title !== '') {
          title = each.metadata.title;
        }

        let app = each['app-name'];
        if (each['app-name'] === 'contacts') {
          app = 'groups';
        };

        if (each['app-name'] === 'graph') {
          app = each.metadata.module;
        }

        const shipStart = each.resource.substr(each.resource.indexOf('~'));

        if (app === 'groups') {
          const obj = result(
            title,
            `/~landscape${each.resource}`,
            app.charAt(0).toUpperCase() + app.slice(1),
            cite(shipStart.slice(0, shipStart.indexOf('/')))
          );
          landscape.push(obj);
        } else {
          const app = each.metadata.module || each['app-name'];
          let group = each.group;
          if (groups[each.group]?.hidden && app === 'chat') {
            group = '/messages';
          } else if (groups[each.group]?.hidden) {
            group = '/home';
          }
          const obj = result(
            title,
            `/~landscape${group}/join/${app}${each.resource}`,
            app.charAt(0).toUpperCase() + app.slice(1),
            (associations?.groups?.[each.group]?.metadata?.title || null)
          );
          subscriptions.push(obj);
        }
      });
  });

  indexes.set('commands', commandIndex(currentGroup, groups, associations));
  indexes.set('subscriptions', subscriptions);
  indexes.set('groups', landscape);
  indexes.set('apps', appIndex(apps));
  indexes.set('other', otherIndex());

  return indexes;
};
