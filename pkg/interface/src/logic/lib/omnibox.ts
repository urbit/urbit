import { isChannelAdmin } from '~/logic/lib/group';
import { cite } from '~/logic/lib/util';
import { createJoinParams } from '~/views/landscape/components/Join/Join';

const makeIndexes = () => new Map([
    ['ships', []],
    ['commands', []],
    ['subscriptions', []],
    ['groups', []],
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
  commands.push(result('Create Group', '/~landscape/new', 'Uqbar Ui', null));
  if (canAdd) {
    commands.push(result('Create Channel', `/~landscape${workspace}/new`, 'Uqbar UI', null));
  }
  commands.push(result('Join Group', '?join-kind=group', 'Uqbar UI', null));

  return commands;
};

const otherIndex = function(config) {
  const other = [];
  const idx = {
    profile: result('Profile', `/~profile/~${window.ship}`, 'profile', null),
    updates: result('Notifications', '/~notifications', 'notifications', null),
    messages: result('Messages', '/~landscape/messages', 'messages', null),
    logout: result('Log Out', '/~/logout', 'logout', null)
  };
  for(const cat of config.categories) {
    if(idx[cat]) {
      other.push(idx[cat]);
    }
  }

  return other;
};

export default function index(contacts, associations, currentGroup, groups, hide): Map<string, OmniboxItem[]> {
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
            cite(shipStart.slice(0, shipStart.indexOf('/')), true) as string
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
  indexes.set('other', otherIndex(hide));

  return indexes;
}
