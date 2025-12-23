import React, { useState } from 'react';
import { useAppStore } from '../store/AppStore';
import { CheckCircle, Circle, MoreVertical, ArrowRight, Clock, Calendar as CalendarIcon, Inbox } from 'lucide-react';
import type { Task } from '../types';

export const FocusView: React.FC = () => {
    const { activeContextId, contexts, setContext, tasks, updateTask, moveTask } = useAppStore();

    // Filter tasks for active context
    const contextTasks = tasks.filter(t => t.context_id === activeContextId);

    const nowTask = contextTasks.find(t => t.status === 'doing');
    const todayTasks = contextTasks.filter(t => t.when === 'today' && t.status !== 'done' && t.status !== 'doing').sort((a, b) => a.order_index - b.order_index);
    const extraTasks = contextTasks.filter(t => t.when === 'today_extra' && t.status !== 'done' && t.status !== 'doing');

    const handleStartTask = (id: string) => {
        updateTask(id, { status: 'doing' });
    };

    const handleCompleteTask = (id: string) => {
        updateTask(id, { status: 'done' });
    };

    const handleMoveTask = (id: string, bucket: 'today_extra' | 'tomorrow' | 'this_week' | 'someday') => {
        moveTask(id, bucket);
    };

    return (
        <div style={{ paddingBottom: '100px' }}>
            {/* Context Switcher */}
            <header style={{ display: 'flex', gap: '0.5rem', marginBottom: '2rem', justifyContent: 'center', flexWrap: 'wrap' }}>
                {contexts.map(ctx => (
                    <button
                        key={ctx.id}
                        onClick={() => setContext(ctx.id)}
                        style={{
                            padding: '0.5rem 1rem',
                            borderRadius: '20px',
                            border: 'none',
                            background: activeContextId === ctx.id ? `var(--theme-${ctx.theme}-primary)` : 'rgba(0,0,0,0.05)',
                            color: activeContextId === ctx.id ? 'white' : 'var(--text-secondary)',
                            fontWeight: 600,
                            cursor: 'pointer',
                            transition: 'all 0.2s',
                            boxShadow: activeContextId === ctx.id ? '0 4px 12px rgba(0,0,0,0.1)' : 'none'
                        }}
                    >
                        {ctx.name}
                    </button>
                ))}
            </header>

            {/* Now Card */}
            <div className="glass-panel" style={{ padding: '2rem', textAlign: 'center', marginBottom: '2rem', position: 'relative', overflow: 'hidden' }}>
                <div style={{
                    position: 'absolute', top: 0, left: 0, right: 0, height: '4px',
                    background: nowTask ? 'var(--active-theme-primary)' : 'transparent'
                }} />

                <h2 style={{ margin: '0 0 1rem 0', color: 'var(--text-secondary)', fontSize: '0.9rem', textTransform: 'uppercase', letterSpacing: '1px' }}>
                    Now
                </h2>

                {nowTask ? (
                    <div style={{ animation: 'fadeIn 0.3s ease' }}>
                        <h3 style={{ fontSize: '1.5rem', margin: '0 0 1.5rem 0', lineHeight: 1.3 }}>{nowTask.title}</h3>
                        <div style={{ display: 'flex', gap: '1rem', justifyContent: 'center' }}>
                            <button
                                className="btn-primary"
                                onClick={() => handleCompleteTask(nowTask.id)}
                                style={{ display: 'flex', alignItems: 'center', gap: '0.5rem', padding: '0.8rem 1.5rem' }}
                            >
                                <CheckCircle size={20} />
                                Done
                            </button>
                            <button
                                className="btn-ghost"
                                onClick={() => updateTask(nowTask.id, { status: 'todo' })}
                                style={{ display: 'flex', alignItems: 'center', gap: '0.5rem' }}
                            >
                                Stop
                            </button>
                        </div>
                    </div>
                ) : (
                    <div style={{ color: 'var(--text-secondary)' }}>
                        {todayTasks.length > 0 ? (
                            <p>Select a task below to focus</p>
                        ) : (
                            <p>No tasks for today. Check your Plan.</p>
                        )}
                    </div>
                )}
            </div>

            {/* Today's 3 */}
            <div style={{ marginBottom: '2rem' }}>
                <h3 style={{ marginLeft: '0.5rem', marginBottom: '1rem', display: 'flex', alignItems: 'center', gap: '0.5rem' }}>
                    Today's Focus
                    <span style={{ fontSize: '0.8rem', color: 'var(--text-secondary)', fontWeight: 'normal' }}>
                        ({todayTasks.length + (nowTask?.when === 'today' ? 1 : 0)}/3)
                    </span>
                </h3>

                <div style={{ display: 'flex', flexDirection: 'column', gap: '0.8rem' }}>
                    {todayTasks.map(task => (
                        <TaskItem
                            key={task.id}
                            task={task}
                            onStart={() => handleStartTask(task.id)}
                            onComplete={() => handleCompleteTask(task.id)}
                            onMove={handleMoveTask}
                        />
                    ))}
                    {todayTasks.length === 0 && !nowTask && (
                        <div style={{ padding: '1rem', textAlign: 'center', color: 'var(--text-secondary)', fontStyle: 'italic' }}>
                            No tasks planned for today
                        </div>
                    )}
                </div>
            </div>

            {/* If There's Time */}
            {extraTasks.length > 0 && (
                <div>
                    <h3 style={{ marginLeft: '0.5rem', marginBottom: '1rem', color: 'var(--text-secondary)', fontSize: '1rem' }}>If There's Time</h3>
                    <div style={{ display: 'flex', flexDirection: 'column', gap: '0.8rem' }}>
                        {extraTasks.map(task => (
                            <TaskItem
                                key={task.id}
                                task={task}
                                isExtra
                                onStart={() => handleStartTask(task.id)}
                                onComplete={() => handleCompleteTask(task.id)}
                                onMove={handleMoveTask}
                            />
                        ))}
                    </div>
                </div>
            )}
        </div>
    );
};

const TaskItem: React.FC<{
    task: Task;
    isExtra?: boolean;
    onStart: () => void;
    onComplete: () => void;
    onMove: (id: string, bucket: 'today_extra' | 'tomorrow' | 'this_week' | 'someday') => void;
}> = ({ task, isExtra, onStart, onComplete, onMove }) => {
    const [showMenu, setShowMenu] = useState(false);

    return (
        <div className="glass-panel" style={{
            padding: '1rem',
            display: 'flex',
            alignItems: 'center',
            gap: '1rem',
            opacity: isExtra ? 0.8 : 1,
            transition: 'transform 0.2s',
            cursor: 'pointer'
        }}
            onClick={(e) => {
                // If clicking the container (not buttons), start the task
                if (e.target === e.currentTarget || (e.target as HTMLElement).tagName === 'DIV' || (e.target as HTMLElement).tagName === 'H4') {
                    onStart();
                }
            }}
        >
            <button
                onClick={(e) => { e.stopPropagation(); onComplete(); }}
                style={{ background: 'none', border: 'none', cursor: 'pointer', color: 'var(--text-secondary)' }}
            >
                <Circle size={24} />
            </button>

            <div style={{ flex: 1 }}>
                <h4 style={{ margin: 0, fontSize: '1rem', fontWeight: 500 }}>{task.title}</h4>
                {task.is_week_priority && (
                    <span style={{ fontSize: '0.7rem', background: 'var(--active-theme-bg)', color: 'var(--active-theme-primary)', padding: '2px 6px', borderRadius: '4px', marginTop: '4px', display: 'inline-block' }}>
                        Weekly Priority
                    </span>
                )}
            </div>

            <div style={{ position: 'relative' }}>
                <button
                    className="btn-ghost"
                    onClick={(e) => { e.stopPropagation(); setShowMenu(!showMenu); }}
                >
                    <MoreVertical size={20} />
                </button>

                {showMenu && (
                    <>
                        <div
                            style={{ position: 'fixed', inset: 0, zIndex: 10 }}
                            onClick={(e) => { e.stopPropagation(); setShowMenu(false); }}
                        />
                        <div style={{
                            position: 'absolute',
                            right: 0,
                            top: '100%',
                            background: 'white',
                            borderRadius: '8px',
                            boxShadow: '0 4px 20px rgba(0,0,0,0.15)',
                            zIndex: 20,
                            minWidth: '160px',
                            overflow: 'hidden'
                        }}>
                            <MenuOption icon={<Clock size={16} />} label="If There's Time" onClick={() => onMove(task.id, 'today_extra')} />
                            <MenuOption icon={<ArrowRight size={16} />} label="Tomorrow" onClick={() => onMove(task.id, 'tomorrow')} />
                            <MenuOption icon={<CalendarIcon size={16} />} label="This Week" onClick={() => onMove(task.id, 'this_week')} />
                            <MenuOption icon={<Inbox size={16} />} label="Someday" onClick={() => onMove(task.id, 'someday')} />
                        </div>
                    </>
                )}
            </div>
        </div>
    );
};

const MenuOption: React.FC<{ icon: React.ReactNode; label: string; onClick: () => void }> = ({ icon, label, onClick }) => (
    <button
        onClick={(e) => { e.stopPropagation(); onClick(); }}
        style={{
            display: 'flex',
            alignItems: 'center',
            gap: '0.8rem',
            width: '100%',
            padding: '0.8rem 1rem',
            border: 'none',
            background: 'transparent',
            textAlign: 'left',
            cursor: 'pointer',
            fontSize: '0.9rem',
            color: 'var(--text-primary)'
        }}
        onMouseEnter={(e) => e.currentTarget.style.background = 'var(--bg-color)'}
        onMouseLeave={(e) => e.currentTarget.style.background = 'transparent'}
    >
        {icon}
        {label}
    </button>
);
