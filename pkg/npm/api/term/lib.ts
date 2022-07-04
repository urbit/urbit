import { Scry } from '../../http-api/src'
import { Poke } from '../../http-api/src/types';
import { Belt, Task, SessionTask } from './types';

const beltToNoun = (bet) => {
  if (typeof bet === 'string') {
    return bet;
  } else
  if ('aro' in bet) {
    return ['aro', bet.aro];
  } else
  if ('bac' in bet) {
    return ['bac', 0];
  } else
  if ('del' in bet) {
    return ['del', 0];
  } else
  if ('hit' in bet) {
    return ['hit', bet.hit.x, bet.hit.y];
  } else
  if ('ret' in bet) {
    return ['ret', 0];
  } else
  if ('mod' in bet) {
    return ['mod', bet.mod.mod, beltToNoun(bet.mod.key)];
  } else
  if ('txt' in bet) {
    return ['txt', [...bet.txt, 0]];
  } else {
    console.log('strange belt', bet);
    return 0;
  }
}

export const pokeTask = (session: string, task: Task): Poke<SessionTask> => {
  let non: any = 0;
  if ('belt' in task) {
    // [%belt p=belt]
    non = ['belt', beltToNoun(task.belt)];
  } else
  if ('blew' in task) {
    // [%blew p=blew]
    non = ['blew', task.blew.w, task.blew.h];
  } else
  if ('hail' in task) {
    // [%hail ~]
    non = ['hail', 0];
  } else
  if ('open' in task) {
    // [%open p=dude:gall q=(list gill:gall)]
    non = ['open', task.open.term, [...task.open.apps.map(a => {
      return [a.who, a.app];  //TODO  a.who as Atom
    }), 0]];
  } else
  if ('shut' in task) {
    // [%shut ~]
    non = ['shut', 0];
  }

  return {
    app: 'herm',
    mark: 'herm-task',
    noun: [session, non],
  }
};

export const pokeBelt = (
  session: string,
  belt: Belt
): Poke<SessionTask> => pokeTask(session, { belt });

//NOTE  scry will return string[]
export const scrySessions = (): Scry => ({
  app: 'herm',
  path: `/sessions`
});
