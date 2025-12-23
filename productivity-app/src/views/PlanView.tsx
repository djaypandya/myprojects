import React, { useState } from 'react';
import { useAppStore } from '../store/AppStore';
import { Plus, MoreVertical, ArrowRight, Calendar as CalendarIcon, Inbox, Trash2 } from 'lucide-react';
import type { Task } from '../types';

export const PlanView: React.FC = () => {
    const [activeTab, setActiveTab] = useState<'today' | 'tomorrow' | 'week'>('today');
    const { contexts, tasks, addTask, updateTask, moveTask, canAddToday, canAddWeekPriority } = useAppStore();

    // Helper to get tasks for a context
    const getTasks = (contextId: string) => tasks.filter(t => t.context_id === contextId);

    const handleAddToday = (contextId: string) => {
        if (!canAddToday(contextId)) {
            alert(`You already have 3 Today tasks for this context. Move one to "If There's Time" first.`);
            return;
        }
        const title = prompt('Task title:');
        if (title) {
            addTask({
                context_id: contextId,
                title,
                status: 'todo',
                when: 'today',
                is_week_priority: false,
                order_index: Date.now()
            });
        }
    };

    const handleAddWeekPriority = (contextId: string) => {
        if (!canAddWeekPriority(contextId)) {
            alert(`You already have 3 Weekly priorities for this context.`);
            return;
        }
        const title = prompt('Priority title:');
        if (title) {
            addTask({
                context_id: contextId,
                title,
                status: 'todo',
                when: 'this_week',
                is_week_priority: true,
                order_index: Date.now()
            });
        }
    };

    return (
        <div style={{ paddingBottom: '100px' }}>
            {/* Tabs */}
            <div style={{ display: 'flex', justifyContent: 'center', marginBottom: '2rem', gap: '0.5rem' }}>
                <button
                    onClick={() => setActiveTab('today')}
                    style={{
                        padding: '0.5rem 1.5rem',
                        borderRadius: '20px',
                        border: 'none',
                        background: activeTab === 'today' ? 'var(--text-primary)' : 'transparent',
                        color: activeTab === 'today' ? 'var(--bg-color)' : 'var(--text-secondary)',
                        fontWeight: 600,
                        cursor: 'pointer',
                        transition: 'all 0.2s'
                    }}
                >
                    Today
                </button>
                <button
                    onClick={() => setActiveTab('tomorrow')}
                    style={{
                        padding: '0.5rem 1.5rem',
                        borderRadius: '20px',
                        border: 'none',
                        background: activeTab === 'tomorrow' ? 'var(--text-primary)' : 'transparent',
                        color: activeTab === 'tomorrow' ? 'var(--bg-color)' : 'var(--text-secondary)',
                        fontWeight: 600,
                        cursor: 'pointer',
                        transition: 'all 0.2s'
                    }}
                >
                    Tomorrow
                </button>
                <button
                    onClick={() => setActiveTab('week')}
                    style={{
                        padding: '0.5rem 1.5rem',
                        borderRadius: '20px',
                        border: 'none',
                        background: activeTab === 'week' ? 'var(--text-primary)' : 'transparent',
                        color: activeTab === 'week' ? 'var(--bg-color)' : 'var(--text-secondary)',
                        fontWeight: 600,
                        cursor: 'pointer',
                        transition: 'all 0.2s'
                    }}
                >
                    This Week
                </button>
            </div>

            {/* Content */}
            <div style={{ display: 'flex', flexDirection: 'column', gap: '2rem' }}>
                {contexts.map(ctx => (
                    <div key={ctx.id}>
                        <h3 style={{
                            color: `var(--theme-${ctx.theme}-primary)`,
                            borderBottom: `2px solid var(--theme-${ctx.theme}-bg)`,
                            paddingBottom: '0.5rem',
                            marginBottom: '1rem'
                        }}>
                            {ctx.name}
                        </h3>

                        {activeTab === 'today' && (
                            <>
                                {/* Today List */}
                                <div style={{ marginBottom: '1rem' }}>
                                    <div style={{ display: 'flex', justifyContent: 'space-between', alignItems: 'center', marginBottom: '0.5rem' }}>
                                        <h4 style={{ margin: 0, fontSize: '0.9rem', color: 'var(--text-secondary)' }}>Today's 3</h4>
                                        <button
                                            className="btn-ghost"
                                            onClick={() => handleAddToday(ctx.id)}
                                            style={{ padding: '4px', height: 'auto' }}
                                        >
                                            <Plus size={16} />
                                        </button>
                                    </div>
                                    <div style={{ display: 'flex', flexDirection: 'column', gap: '0.5rem' }}>
                                        {getTasks(ctx.id)
                                            .filter(t => t.when === 'today' && t.status !== 'done')
                                            .map(t => (
                                                <PlanTaskItem key={t.id} task={t} updateTask={updateTask} moveTask={moveTask} />
                                            ))}
                                    </div>
                                </div>

                                {/* Today Extra */}
                                <div>
                                    <h4 style={{ margin: '0 0 0.5rem 0', fontSize: '0.9rem', color: 'var(--text-secondary)' }}>If There's Time</h4>
                                    <div style={{ display: 'flex', flexDirection: 'column', gap: '0.5rem' }}>
                                        {getTasks(ctx.id)
                                            .filter(t => t.when === 'today_extra' && t.status !== 'done')
                                            .map(t => (
                                                <PlanTaskItem key={t.id} task={t} updateTask={updateTask} moveTask={moveTask} />
                                            ))}
                                    </div>
                                </div>
                            </>
                        )}

                        {activeTab === 'tomorrow' && (
                            <>
                                {/* Tomorrow */}
                                <div>
                                    <h4 style={{ margin: '0 0 0.5rem 0', fontSize: '0.9rem', color: 'var(--text-secondary)' }}>Tomorrow</h4>
                                    <div style={{ display: 'flex', flexDirection: 'column', gap: '0.5rem' }}>
                                        {getTasks(ctx.id)
                                            .filter(t => t.when === 'tomorrow' && t.status !== 'done')
                                            .map(t => (
                                                <PlanTaskItem key={t.id} task={t} updateTask={updateTask} moveTask={moveTask} />
                                            ))}
                                        {getTasks(ctx.id).filter(t => t.when === 'tomorrow' && t.status !== 'done').length === 0 && (
                                            <div style={{ fontSize: '0.8rem', color: 'var(--text-secondary)', fontStyle: 'italic', opacity: 0.7 }}>
                                                No tasks
                                            </div>
                                        )}
                                    </div>
                                </div>
                            </>
                        )}

                        {activeTab === 'week' && (
                            <>
                                {/* Week Priorities */}
                                <div>
                                    <div style={{ display: 'flex', justifyContent: 'space-between', alignItems: 'center', marginBottom: '0.5rem' }}>
                                        <h4 style={{ margin: 0, fontSize: '0.9rem', color: 'var(--text-secondary)' }}>Weekly Priorities</h4>
                                        <button
                                            className="btn-ghost"
                                            onClick={() => handleAddWeekPriority(ctx.id)}
                                            style={{ padding: '4px', height: 'auto' }}
                                        >
                                            <Plus size={16} />
                                        </button>
                                    </div>
                                    <div style={{ display: 'flex', flexDirection: 'column', gap: '0.5rem' }}>
                                        {getTasks(ctx.id)
                                            .filter(t => t.is_week_priority && t.status !== 'done')
                                            .map(t => (
                                                <PlanTaskItem key={t.id} task={t} updateTask={updateTask} moveTask={moveTask} />
                                            ))}
                                    </div>
                                </div>
                            </>
                        )}
                    </div>
                ))}
            </div>
        </div>
    );
};

const PlanTaskItem: React.FC<{
    task: Task;
    updateTask: any;
    moveTask: any;
}> = ({ task, updateTask, moveTask }) => {
    const [showMenu, setShowMenu] = useState(false);

    return (
        <div className="glass-panel" style={{ padding: '0.8rem', display: 'flex', alignItems: 'center', gap: '0.8rem' }}>
            <div style={{ flex: 1, fontSize: '0.95rem' }}>{task.title}</div>

            <div style={{ position: 'relative' }}>
                <button
                    className="btn-ghost"
                    onClick={() => setShowMenu(!showMenu)}
                    style={{ padding: '4px' }}
                >
                    <MoreVertical size={16} />
                </button>

                {showMenu && (
                    <>
                        <div style={{ position: 'fixed', inset: 0, zIndex: 10 }} onClick={() => setShowMenu(false)} />
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
                            <MenuOption icon={<ArrowRight size={16} />} label="Tomorrow" onClick={() => moveTask(task.id, 'tomorrow')} />
                            <MenuOption icon={<CalendarIcon size={16} />} label="This Week" onClick={() => moveTask(task.id, 'this_week')} />
                            <MenuOption icon={<Inbox size={16} />} label="Someday" onClick={() => moveTask(task.id, 'someday')} />
                            <div style={{ height: '1px', background: '#eee', margin: '4px 0' }} />
                            <MenuOption icon={<Trash2 size={16} />} label="Delete" onClick={() => updateTask(task.id, { status: 'done' })} />
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
            padding: '0.6rem 1rem',
            border: 'none',
            background: 'transparent',
            textAlign: 'left',
            cursor: 'pointer',
            fontSize: '0.85rem',
            color: 'var(--text-primary)'
        }}
        onMouseEnter={(e) => e.currentTarget.style.background = 'var(--bg-color)'}
        onMouseLeave={(e) => e.currentTarget.style.background = 'transparent'}
    >
        {icon}
        {label}
    </button>
);
