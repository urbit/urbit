import { isChannelAdmin } from '~/logic/lib/group';
import { cite } from '~/logic/lib/util';

const makeIndexes = () => new Map([
    ['ships', []],
    ['commands', []],
    ['subscriptions', []],
    ['groups', []],
    ['apps', []],
    ['other', []]
  ]);

export interface OmniboxItem {
  title: string;
  link: string;
  app: string;
  host: string;
  description: string;
  shiftLink: string;
  shiftDescription: string;
}

// result schematic
const result = function(title: string, link: string, app: string, host: string, description = 'Open', shiftLink = null as string | null, shiftDescription = null as string | null): OmniboxItem {
  return {
    'title': title,
    'link': link,
    'app': app,
    'host': host,
    'description': description,
    'shiftLink': shiftLink,
    'shiftDescription': shiftDescription

  };
};

const shipIndex = function(contacts) {
  const ships = [];
  Object.keys(contacts).map((e) => {
    return ships.push(result(e, `/~profile/${e}`, 'profile', contacts[e]?.status || '', 'Open Profile', `/~landscape/messages/dm/${e}`, 'Send Message'));
  });
  return ships;
};

const commandIndex = function (currentGroup, groups, associations) {
  // commands are special cased for default suite
  const commands = [];
  const group = currentGroup ? groups[currentGroup] : null;
  const association = currentGroup ? associations?.groups?.[currentGroup] : null;
  const canAdd =
    (group && association)
    ? (association.metadata.vip === 'member-metadata' || isChannelAdmin(group, currentGroup))
    : !currentGroup; // home workspace or hasn't loaded
  const workspace = currentGroup || '/home';
  commands.push(result('Groups: Create', '/~landscape/new', 'Groups', null));
  if (canAdd) {
    commands.push(result('Channel: Create', `/~landscape${workspace}/new`, 'Groups', null));
  }
  commands.push(result('Groups: Join', '/~landscape/join', 'Groups', null));

  return commands;
};

const appIndex = function (apps) {
  // all apps are indexed from launch data
  // indexed into 'apps'
  const applications = [];
  Object.keys(apps)
    .filter((e) => {
      return !['weather','clock'].includes(e);
    })
    .sort((a, b) => {
      return a.localeCompare(b);
    })
    .map((e) => {
      const obj = result(
        apps[e].type?.basic?.title || apps[e].type.custom?.tile || e,
        apps[e]?.type.basic?.linkedUrl || apps[e]?.type.custom?.linkedUrl || '',
        apps[e]?.type?.basic?.title || apps[e].type.custom?.tile || e,
        null
      );
      applications.push(obj);
    });
  return applications;
};

const otherIndex = function(config) {
  const other = [];
  const idx = {
    mychannel: result('My Channels', '/~landscape/home', 'home', null),
    updates: result('Notifications', '/~notifications', 'inbox', null),
    profile: result('Profile', `/~profile/~${window.ship}`, 'profile', null),
    messages: result('Messages', '/~landscape/messages', 'messages', null),
    logout: result('Log Out', '/~/logout', 'logout', null)
  };
  other.push(result('Tutorial', '/?tutorial=true', 'tutorial', null));
  for(const cat of config.categories) {
    if(idx[cat]) {
      other.push(idx[cat]);
    }
  }

  return other;
};

export default function index(contacts, associations, apps, currentGroup, groups, hide): Map<string, OmniboxItem[]> {
  const indexes = makeIndexes();
  indexes.set('ships', shipIndex(contacts));
  // all metadata from all apps is indexed
  // into subscriptions and landscape
  const subscriptions = [];
  const landscape = [];
  Object.keys(associations).filter((e) => {
    // skip apps with no metadata
    return Object.keys(associations[e]).length > 0;
  }).map((e) => {
    // iterate through each app's metadata object
    Object.keys(associations[e])
      .filter(association => !associations?.[e]?.[association]?.metadata?.hidden)
      .map((association) => {
        const each = associations[e][association];
        let title = each.resource;
        if (each.metadata.title !== '') {
          title = each.metadata.title;
        }

        let app = each['app-name'];
        if (each['app-name'] === 'contacts') {
          app = 'groups';
        }

        if (each['app-name'] === 'graph') {
          app = each.metadata.config.graph;
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
          const app = each.metadata.config.graph || each['app-name'];
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
  indexes.set('other', otherIndex(hide));

  return indexes;
}
