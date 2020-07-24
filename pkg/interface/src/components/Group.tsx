import React, { Component } from 'react';
import _, { capitalize } from 'lodash';
import { Dropdown } from '../apps/publish/components/lib/dropdown';
import { cite, deSig } from '../lib/util';
import { roleForShip, resourceFromPath } from '../lib/group';
import {
  Group,
  InvitePolicy,
  OpenPolicy,
  roleTags,
  Groups,
} from '../types/group-update';
import { Path, PatpNoSig, Patp } from '../types/noun';
import GlobalApi from '../api/global';
import { Menu, MenuButton, MenuList, MenuItem } from '@tlon/indigo-react';
import InviteSearch, { Invites } from './InviteSearch';
import { Spinner } from './Spinner';
import { Rolodex } from '../types/contact-update';
import { Associations } from '../types/metadata-update';

class GroupMember extends Component<{ ship: Patp; options: any[] }, {}> {
  render() {
    const { ship, options, children } = this.props;

    return (
      <div className='flex justify-between f9 items-center'>
        <div className='flex flex-column'>
          <div className='mono mr2'>{`${cite(ship)}`}</div>
          {children}
        </div>
        {options.length > 0 && (
          <Menu>
            <MenuButton sm>Options</MenuButton>
            <MenuList>
              {options.map(({ onSelect, text }) => (
                <MenuItem onSelect={onSelect}>{text}</MenuItem>
              ))}
            </MenuList>
          </Menu>
        )}
      </div>
    );
  }
}

class Tag extends Component<{ description: string; onRemove?: () => any }, {}> {
  render() {
    const { description, onRemove } = this.props;
    return (
      <div className='br-pill ba b-black r-full items-center ph2 f9 mr2 flex'>
        <div>{description}</div>
        {Boolean(onRemove) && (
          <div onClick={onRemove} className='ml1 f9 pointer'>
            ✗
          </div>
        )}
      </div>
    );
  }
}

interface GroupViewAppTag {
  tag: string;
  app: string;
  desc: string;
  addDesc: string;
}

interface GroupViewProps {
  group: Group;
  groups: Groups;
  contacts: Rolodex;
  associations: Associations;
  resourcePath: Path;
  appTags?: GroupViewAppTag[];
  api: GlobalApi;
  className: string;
  permissions?: boolean;
  inviteShips: (ships: PatpNoSig[]) => Promise<any>;
}

export class GroupView extends Component<
  GroupViewProps,
  { invites: Invites; awaiting: boolean }
> {
  constructor(props) {
    super(props);
    this.setInvites = this.setInvites.bind(this);
    this.inviteShips = this.inviteShips.bind(this);
    this.state = {
      invites: {
        ships: [],
        groups: [],
      },
      awaiting: false
    };
  }

  removeUser(who: PatpNoSig) {
    return () => {
      const resource = resourceFromPath(this.props.resourcePath);
      this.props.api.groups.remove(resource, [`~${who}`]);
    };
  }

  banUser(who: PatpNoSig) {
    const resource = resourceFromPath(this.props.resourcePath);
    this.props.api.groups.changePolicy(resource, {
      open: {
        banShips: [`~${who}`],
      },
    });
  }

  allowUser(who: PatpNoSig) {
    const resource = resourceFromPath(this.props.resourcePath);
    this.props.api.groups.changePolicy(resource, {
      open: {
        allowShips: [`~${who}`],
      },
    });
  }

  removeInvite(who: PatpNoSig) {
    const resource = resourceFromPath(this.props.resourcePath);
    this.props.api.groups.changePolicy(resource, {
      invite: {
        removeInvites: [`~${who}`],
      },
    });
  }

  removeTag(who: PatpNoSig, tag: any) {
    const resource = resourceFromPath(this.props.resourcePath);

    return this.props.api.groups.removeTag(resource, tag, [`~${who}`]);
  }

  addTag(who: PatpNoSig, tag: any) {
    const resource = resourceFromPath(this.props.resourcePath);
    return this.props.api.groups.addTag(resource, tag, [`~${who}`]);
  }

  isAdmin(): boolean {
    const us = `~${window.ship}`;
    const role = roleForShip(this.props.group, us);
    const resource = resourceFromPath(this.props.resourcePath);
    return resource.ship == us || role === 'admin';
  }

  optionsForShip(ship: Patp, missing: GroupViewAppTag[]) {
    const { permissions, resourcePath, group } = this.props;
    const resource = resourceFromPath(resourcePath);
    let options: any[] = [];
    if (!permissions) {
      return options;
    }
    const role = roleForShip(group, ship);
    if (role === 'admin' || resource.ship === ship) {
      return [];
    }
    if ('open' in group.policy) {
      options.unshift({ text: 'Ban', onSelect: () => this.banUser(ship) });
    }
    if (this.isAdmin() && !role) {
      options = options.concat(
        missing.map(({ addDesc, tag, app }) => ({
          text: addDesc,
          onSelect: () => this.addTag(ship, { tag, app }),
        }))
      );
      options = options.concat(
        roleTags.reduce(
          (acc, role) => [
            ...acc,
            {
              text: `Make ${capitalize(role)}`,
              onSelect: () => this.addTag(ship, { tag: role }),
            },
          ],
          [] as any[]
        )
      );
    }

    return options;
  }

  doIfAdmin<Ret>(f: () => Ret) {
    return this.isAdmin() ? f : undefined;
  }

  getAppTags(ship: Patp): [GroupViewAppTag[], GroupViewAppTag[]] {
    const { tags } = this.props.group;
    const { appTags } = this.props;

    return _.partition(appTags, ({ app, tag }) => {
      return tags?.[app]?.[tag]?.has(ship);
    });
  }

  renderMembers() {
    const { group, permissions } = this.props;
    const { members } = group;
    const isAdmin = this.isAdmin();
    return (
      <div className='flex flex-column'>
        <div className='f9 gray2 mt6 mb3'>Members</div>
        {Array.from(members).map((ship) => {
          const role = roleForShip(group, deSig(ship));
          const onRoleRemove =
            role && isAdmin
              ? () => {
                  this.removeTag(ship, { tag: role });
                }
              : undefined;
          const [present, missing] = this.getAppTags(ship);
          const options = this.optionsForShip(ship, missing);

          return (
            <div key={ship} className='flex flex-column pv3'>
              <GroupMember ship={ship} options={options}>
                {((permissions && role) || present.length > 0) && (
                  <div className='flex mt1'>
                    {role && (
                      <Tag
                        onRemove={onRoleRemove}
                        description={capitalize(role)}
                      />
                    )}
                    {present.map((tag, idx) => (
                      <Tag
                        key={idx}
                        onRemove={this.doIfAdmin(() =>
                          this.removeTag(ship, tag)
                        )}
                        description={tag.desc}
                      />
                    ))}
                  </div>
                )}
              </GroupMember>
            </div>
          );
        })}
      </div>
    );
  }

  setInvites(invites: Invites) {
    this.setState({ invites });
  }

  inviteShips(invites: Invites) {
    const { props, state } = this;
    this.setState({ awaiting: true });
    props.inviteShips(invites.ships).then(() => {
      this.setState({ invites: { ships: [], groups: [] }, awaiting: false });
    });
  }

  renderInvites(policy: InvitePolicy) {
    const { props, state } = this;
    const ships = Array.from(policy.invite.pending || []);

    const options = (ship: Patp) => [
      { text: 'Uninvite', onSelect: () => this.removeInvite(ship) },
    ];

    return (
      <div className='flex flex-column'>
        <div className='f9 gray2 mt6 mb3'>Pending</div>
        {ships.map((ship) => (
          <GroupMember key={ship} ship={ship} options={options(ship)} />
        ))}
        {ships.length === 0 && <div className='f9'>No ships are pending</div>}
        {props.inviteShips && this.isAdmin() && (
          <>
            <div className='f9 gray2 mt6 mb3'>Invite</div>
            <div style={{ width: 'calc(min(400px, 100%)' }}>
              <InviteSearch
                groups={props.groups}
                contacts={props.contacts}
                shipResults
                groupResults={false}
                invites={state.invites}
                setInvite={this.setInvites}
                associations={props.associations}
              />
            </div>
            <a
              onClick={() => this.inviteShips(state.invites)}
              className='db ba tc w-auto mr-auto mt2 ph2 black white-d f8 pointer'
            >
              Invite
            </a>
          </>
        )}
      </div>
    );
  }

  renderBanned(policy: OpenPolicy) {
    const ships = Array.from(policy.open.banned || []);

    const options = (ship: Patp) => [
      { text: 'Unban', onSelect: () => this.allowUser(ship) },
    ];

    return (
      <div className='flex flex-column'>
        <div className='f9 gray2 mt6 mb3'>Banned</div>
        {ships.map((ship) => (
          <GroupMember key={ship} ship={ship} options={options(ship)} />
        ))}
        {ships.length === 0 && <div className='f9'>No ships are banned</div>}
      </div>
    );
  }

  render() {
    const { group, resourcePath, className } = this.props;
    const resource = resourceFromPath(resourcePath);

    return (
      <div className={className}>
        <div className='flex flex-column'>
          <div className='f9 gray2'>Host</div>
          <div className='flex justify-between mt3'>
            <div className='f9 mono mr2'>{cite(resource.ship)}</div>
          </div>
        </div>
        {'invite' in group.policy && this.renderInvites(group.policy)}
        {'open' in group.policy && this.renderBanned(group.policy)}
        {this.renderMembers()}

        <Spinner
          awaiting={this.state.awaiting}
          classes='mt4'
          text='Inviting to chat...'
        />
      </div>
    );
  }
}
